/* This file is part of RTags (http://rtags.net).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

#include "JobScheduler.h"

#include "IndexDataMessage.h"
#include "IndexerJob.h"
#include "Project.h"
#include "rct/Connection.h"
#include "rct/Process.h"
#include "Server.h"
#ifdef RP_USE_THREAD
#include "RPThread.h"
#endif

enum { MaxPriority = 10 };
// we set the priority to be this when a job has been requested and we couldn't load it
JobScheduler::JobScheduler()
    : mProcrastination(0)
{}

JobScheduler::~JobScheduler()
{
    mPendingJobs.deleteAll();
    if (!mActiveByVehicle.isEmpty()) {
        for (const auto &job : mActiveByVehicle) {
            job.first->kill();
            delete job.first;
        }
    }
}

void JobScheduler::add(const std::shared_ptr<IndexerJob> &job)
{
    assert(!(job->flags & ~IndexerJob::Type_Mask));
    std::shared_ptr<Node> node(new Node({ 0, job, 0, 0, 0, String() }));
    node->job = job;
    // error() << job->priority << job->sourceFile << mProcrastination;
    if (mPendingJobs.isEmpty() || job->priority() > mPendingJobs.first()->job->priority()) {
        mPendingJobs.prepend(node);
    } else {
        std::shared_ptr<Node> after = mPendingJobs.last();
        while (job->priority() > after->job->priority()) {
            after = after->prev;
            assert(after);
        }
        mPendingJobs.insert(node, after);
    }
    assert(!mInactiveById.contains(job->id));
    mInactiveById[job->id] = node;
    // error() << "procrash" << mProcrastination << job->sourceFile;
    if (!mProcrastination)
        startJobs();
}

void JobScheduler::startJobs()
{
    Server *server = Server::instance();
    assert(server);
    if (server->suspended()) {
        warning() << "Suspended, not starting jobs";
        return;
    }
    const auto &options = server->options();
    std::shared_ptr<Node> jobNode = mPendingJobs.first();
    auto cont = [&jobNode, this]() {
        auto tmp = jobNode->next;
        mPendingJobs.remove(jobNode);
        jobNode = tmp;
    };

    while (mActiveByVehicle.size() < options.jobCount && jobNode) {
        assert(jobNode);
        assert(jobNode->job);
        assert(!(jobNode->job->flags & (IndexerJob::Running|IndexerJob::Complete|IndexerJob::Crashed|IndexerJob::Aborted)));
        std::shared_ptr<Project> project = Server::instance()->project(jobNode->job->project);
        if (!project) {
            cont();
            debug() << jobNode->job->sourceFile << "doesn't have a project, discarding";
            continue;
        }

        const uint64_t jobId = jobNode->job->id;
        Vehicle *vehicle = new Vehicle;
        debug() << "Starting vehicle for" << jobId << jobNode->job->fileId() << jobNode->job.get();
        List<String> arguments;
        arguments << "--priority" << String::number(jobNode->job->priority());

        for (int i=logLevel().toInt(); i>0; --i)
            arguments << "-v";

        vehicle->readyReadStdOut().connect([this](Vehicle *proc) {
                std::shared_ptr<Node> n = mActiveByVehicle[proc];
                assert(n);
                n->stdOut.append(proc->readAllStdOut());

                std::regex rx("@CRASH@([^@]*)@CRASH@");
                std::smatch match;
                while (std::regex_search(n->stdOut.ref(), match, rx)) {
                    error() << match[1].str();
                    n->stdOut.remove(match.position(), match.length());
                }
            });

        if (!vehicle->start(options.rp, arguments)) {
            error() << "Couldn't start rp" << options.rp << vehicle->errorString();
            delete vehicle;
            jobNode->job->flags |= IndexerJob::Crashed;
            debug() << "job crashed (didn't start)" << jobId << jobNode->job->fileId() << jobNode->job.get();
            auto msg = std::make_shared<IndexDataMessage>(jobNode->job);
            msg->setFlag(IndexDataMessage::ParseFailure);
            jobFinished(jobNode->job, msg);
            cont();
            continue;
        }
        const int pid = vehicle->pid();
        vehicle->finished().connect([this, jobId, options, pid](Vehicle *proc) {
                EventLoop::deleteLater(proc);
                auto n = mActiveByVehicle.take(proc);
                assert(!n || n->vehicle == proc);
                const String stdErr = proc->readAllStdErr();
                if ((n && !n->stdOut.isEmpty()) || !stdErr.isEmpty()) {
                    error() << (n ? ("Output from " + n->job->sourceFile + ":") : String("Orphaned vehicle:"))
                            << '\n' << stdErr << (n ? n->stdOut : String());
                }
                Path::rmdir(options.tempDir + String::number(pid));

                if (n) {
                    assert(n->vehicle == proc);
                    n->vehicle = 0;
                    assert(!(n->job->flags & IndexerJob::Aborted));
                    if (!(n->job->flags & IndexerJob::Complete) && proc->returnCode() != 0) {
                        auto nodeById = mActiveById.take(jobId);
                        assert(nodeById);
                        assert(nodeById == n);
                        // job failed, probably no IndexDataMessage coming
                        n->job->flags |= IndexerJob::Crashed;
                        debug() << "job crashed" << jobId << n->job->fileId() << n->job.get();
                        auto msg = std::make_shared<IndexDataMessage>(n->job);
                        msg->setFlag(IndexDataMessage::ParseFailure);
                        jobFinished(n->job, msg);
                    }
                }
                startJobs();
            });


        jobNode->vehicle = vehicle;
        assert(!(jobNode->job->flags & ~IndexerJob::Type_Mask));
        jobNode->job->flags |= IndexerJob::Running;
        vehicle->write(jobNode->job->encode());
        jobNode->started = Rct::monoMs();
        mActiveByVehicle[vehicle] = jobNode;
        // error() << "STARTING JOB" << node->job->sourceFile;
        mInactiveById.remove(jobId);
        mActiveById[jobId] = jobNode;
        cont();
    }
}

void JobScheduler::handleIndexDataMessage(const std::shared_ptr<IndexDataMessage> &message)
{
    auto node = mActiveById.take(message->id());
    if (!node) {
        debug() << "Got IndexDataMessage for unknown job";
        return;
    }
    debug() << "job got index data message" << node->job->id << node->job->fileId() << node->job.get();
    jobFinished(node->job, message);
}

void JobScheduler::jobFinished(const std::shared_ptr<IndexerJob> &job, const std::shared_ptr<IndexDataMessage> &message)
{
    assert(!(job->flags & IndexerJob::Aborted));
    assert(job);
    assert(message);
    std::shared_ptr<Project> project = Server::instance()->project(job->project);
    if (!project)
        return;

    job->flags &= ~IndexerJob::Running;
    if (!(job->flags & IndexerJob::Crashed)) {
        job->flags |= IndexerJob::Complete;
    } else {
        ++job->crashCount;
        const auto &options = Server::instance()->options();
        assert(job->crashCount <= options.maxCrashCount);
        if (job->crashCount < options.maxCrashCount) {
            project->releaseFileIds(job->visited);
            EventLoop::eventLoop()->registerTimer([job, this](int) {
                    if (!(job->flags & IndexerJob::Aborted)) {
                        job->flags &= ~IndexerJob::Crashed;
                        job->acquireId();
                        add(job);
                    }
                }, 500, Timer::SingleShot); // give it 500 ms before we try again
            return;
        }
        debug() << "job crashed too many times" << job->id << job->fileId() << job.get();
    }
    project->onJobFinished(job, message);
}

void JobScheduler::dump(const std::shared_ptr<Connection> &conn)
{
    if (!mPendingJobs.isEmpty()) {
        conn->write("Pending:");
        for (const auto &node : mPendingJobs) {
            conn->write<128>("%s: %s %d %s",
                             node->job->sourceFile.constData(),
                             node->job->flags.toString().constData(),
                             node->job->priority(),
                             IndexerJob::dumpFlags(node->job->flags).constData());
        }
    }
    if (!mActiveById.isEmpty()) {
        conn->write("Active:");
        const unsigned long long now = Rct::monoMs();
        for (const auto &node : mActiveById) {
            conn->write<128>("%s: %s priority: %d %s %lldms",
                             node.second->job->sourceFile.constData(),
                             node.second->job->flags.toString().constData(),
                             node.second->job->priority(),
                             IndexerJob::dumpFlags(node.second->job->flags).constData(),
                             now - node.second->started);

        }
    }
}

void JobScheduler::abort(const std::shared_ptr<IndexerJob> &job)
{
    assert(!(job->flags & IndexerJob::Aborted));
    job->flags |= IndexerJob::Aborted;
    if (job->flags & IndexerJob::Crashed) {
        return;
    }
    job->flags &= ~IndexerJob::Running;
    auto node = mActiveById.take(job->id);
    if (!node) {
        debug() << "Aborting inactive job" << job->sourceFile << job->fileId() << job->id << job.get();
        node = mInactiveById.take(job->id);
        assert(node);
        mPendingJobs.remove(node);
    } else {
        debug() << "Aborting active job" << job->sourceFile << job->fileId() << job->id << job.get();
    }
    if (node->vehicle) {
        debug() << "Killing vehicle" << node->vehicle;
        node->vehicle->kill();
        mActiveByVehicle.remove(node->vehicle);
    }
}

void JobScheduler::sort()
{
    std::vector<std::shared_ptr<Node> > nodes(mPendingJobs.size());
    for (size_t i=0; i<nodes.size(); ++i) {
        std::shared_ptr<Node> node = mPendingJobs.removeFirst();
        node->job->recalculatePriority();
        nodes[i] = std::move(node);
    }

    std::stable_sort(nodes.begin(), nodes.end(), [](const std::shared_ptr<Node> &l, const std::shared_ptr<Node> &r) -> bool {
            return l->job->priority() > r->job->priority();
        });

    for (std::shared_ptr<Node> &n : nodes) {
        mPendingJobs.append(std::move(n));
    }
}
