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

#ifndef ClangThread_h
#define ClangThread_h

#include <clang-c/Index.h>

#include "Project.h"
#include "QueryMessage.h"
#include "rct/Thread.h"
#include "rct/Value.h"
#include "Source.h"

class Connection;
struct Dep;
class ClangThread : public Thread
{
public:
    ClangThread(const std::shared_ptr<QueryMessage> &queryMessage,
                const Source &source,
                const std::shared_ptr<Connection> &conn);
    ~ClangThread() override;
    virtual void run() override;
    void abort() { std::unique_lock<std::mutex> lock(mMutex); mAborted = false; }
    bool isAborted() const { std::unique_lock<std::mutex> lock(mMutex); return mAborted; }


private:
    static CXChildVisitResult visitor(CXCursor cursor, CXCursor, CXClientData userData);
    CXChildVisitResult visit(const CXCursor &cursor);
    void checkIncludes(Location location, const CXCursor &cursor);

    void writeToConnetion(const String &message);
    void handleInclude(Location loc, const CXCursor &cursor);
    void handleReference(Location loc, const CXCursor &ref);
    void checkIncludes();

#ifdef RTAGS_HAS_LUA
    static CXChildVisitResult visitASTVisitor(CXCursor cursor, CXCursor, CXClientData userData);
    void processAST(CXTranslationUnit unit);

    enum ExtraDataType {
        Type_Invalid,
        Type_Cursor,
        Type_Location,
        Type_Range,
        Type_Type
    };
#if 0
    template <typename T>
    static T extraData(const std::shared_ptr<ScriptEngine::Object> &object, ExtraDataType type, bool *ok = 0)
    {
        assert(object);
        return object->extraData<T>(type, ok);
    }

    Value create(Loc location)
    {
        auto object = mLocationClass->create();
        object->setExtraData<Loc>(location, Type_Location);
        return mScriptEngine.fromObject(object);
    }

    Value create(CXSourceRange range)
    {
        auto object = mRangeClass->create();
        object->setExtraData<CXSourceRange>(range, Type_Range);
        return mScriptEngine.fromObject(object);
    }

    Value create(CXType type)
    {
        auto object = mTypeClass->create();
        object->setExtraData<CXType>(type, Type_Type);
        return mScriptEngine.fromObject(object);
    }

    Value create(Cursor *cursor)
    {
        if (cursor) {
            assert(cursor);
            assert(cursor->object);
            return mScriptEngine.fromObject(cursor->object);
        }
        return Value();
    }

    Value create(const CXCursor &cursor)
    {
        const CXCursorKind kind = clang_getCursorKind(cursor);
        if (!clang_isInvalid(kind)) {
            const Loc loc = createLocation(cursor);
            auto it = mCursorsByLocation.find(loc);
            if (it != mCursorsByLocation.end()) {
                for (Cursor *c : it->second) {
                    if (clang_equalCursors(c->cursor, cursor))
                        return mScriptEngine.fromObject(c->object);
                }
            }
        }
        return Value();
    }

    Value create(CXString str)
    {
        return RTags::eatString(str);
    }

    template <typename T>
    Value create(const T &t)
    {
        return t;
    }

#endif
#endif
    const std::shared_ptr<QueryMessage> mQueryMessage;
    const Source mSource;
    std::shared_ptr<Connection> mConnection;
    int mIndentLevel;
    mutable std::mutex mMutex;
    Hash<uint32_t, Dep*> mDependencies;
    Hash<Path, String> mContextCache;
    bool mAborted;
};

#endif
