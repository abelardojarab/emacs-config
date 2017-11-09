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

#ifndef IndexerJob_h
#define IndexerJob_h

#include "rct/Flags.h"
#include "rct/SignalSlot.h"
#include "RTags.h"
#include "Source.h"

class IndexerJob
{
public:
    enum Flag {
        None = 0x000,
        Dirty = 0x001,
        Reindex = 0x002,
        Compile = 0x004,
        Running = 0x010,
        Crashed = 0x020,
        Aborted = 0x040,
        Complete = 0x080,
        NoAbort = 0x100,
        Active = 0x200,
        Type_Mask = Dirty|Compile|Reindex
    };

    static String dumpFlags(Flags<Flag> flags);

    IndexerJob(const SourceList &sources,
               Flags<Flag> flags,
               const std::shared_ptr<Project> &project,
               const UnsavedFiles &unsavedFiles = UnsavedFiles());
    ~IndexerJob();

    void acquireId();
    String encode() const;

    uint32_t fileId() const { assert(!sources.isEmpty()); return sources.begin()->fileId; }

    int priority() const;
    void recalculatePriority();

    uint64_t id;
    SourceList sources;
    Path sourceFile;
    Flags<Flag> flags;
    Path project;
    UnsavedFiles unsavedFiles;
    Set<uint32_t> visited;
    int crashCount;
    Signal<std::function<void(IndexerJob *)> > destroyed;

private:
    mutable int mCachedPriority;
    static uint64_t sNextId;
};

RCT_FLAGS(IndexerJob::Flag);

#endif
