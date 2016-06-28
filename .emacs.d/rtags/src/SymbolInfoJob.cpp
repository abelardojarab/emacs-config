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

#include "SymbolInfoJob.h"

#include "Project.h"
#include "QueryMessage.h"
#include "RTags.h"
#include "Server.h"

SymbolInfoJob::SymbolInfoJob(Location s, Location e,
                             const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Project> &proj)
    : QueryJob(query, proj), start(s), end(e)
{
}

int SymbolInfoJob::execute()
{
    int ret = 1;
    int idx = -1;
    if (end.isNull()) {
        auto symbol = project()->findSymbol(start, &idx);
        if (!symbol.isNull()) {
            write(symbol);
            ret = 0;
        }
    } else {
        assert(start.fileId() == end.fileId());
        auto symbols = project()->openSymbols(start.fileId());
        if (symbols && symbols->count()) {
            bool exact = false;
            uint32_t idx = symbols->lowerBound(start, &exact);
            if (exact) {
                write("(list");
                write(symbols->valueAt(idx++));
                ret = 0;
            } else {
                switch (idx) {
                case 0:
                    break;
                case std::numeric_limits<uint32_t>::max():
                    idx = symbols->count() - 1;
                    break;
                default:
                    --idx;
                    break;
                }
            }
            const uint32_t count = symbols->count();
            while (idx < count) {
                const Location loc = symbols->keyAt(idx);
                if (loc > end)
                    break;
                if (loc >= start) {
                    if (ret)
                        write("(list");
                    write(symbols->valueAt(idx));
                    ret = 0;
                }
                ++idx;
            }
            if (!ret)
                write(")");
        }
    }
    return ret;
}
