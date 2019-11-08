//
// Copyright (c) 2017, chunquedong
// Licensed under the LGPL
// History:
//   2017-1-21  Jed Young  Creation
//

class Recover {
  TransPageMgr store

  private static const Log log := Recover#.pod.log

  new make(TransPageMgr s) {
    store = s
  }

  Void undo(Int transId) {
    store.logger.flush
    log.debug("undo trans: $transId")

    pos := store.logger.getTransTail(transId)
    if (pos == -1) return
    undoFrom(pos)
  }

  private Void undoFrom(Int pos) {
    PLogRec? rec := null
    while (pos >= 0) {
      rec = store.logger.readLog(pos)
      log.debug("undo log:$pos $rec")
      if (rec == null) break

      switch (rec.type) {
        case PLogType.updatePage:
          PageLogRec prec := rec
          if (prec.oldData != null) {
            buf := prec.oldData
            page := Page(store.pageSize, prec.pageId, buf.in)
            store.updatePageDirect(page)
          }

        case PLogType.freePage:
          FreePageLogRec frec := rec
          if (store.freePage == frec.firstPage) {
            page := store.getPage(rec.transId, frec.lastPage)
            store.freePage = page.id
            throw Err("unreachable code")
          }

        case PLogType.createPage:
          CreatePageLogRec frec := rec
          page := store.getPage(rec.transId, frec.pageId)
          store.deletePageDirect(page, page)

      }
      if (rec.previous == -1) break
      pos = pos - rec.previous
    }
  }

  Void redo(Int pos) {
    if (pos == -1) return
    [Int:Int] transSet := [:]
    Int lastTrans := -1
    while (pos != -1) {
      //echo("redo pos: $pos")
      rec := store.logger.readLog(pos)
      if (rec == null) break
      log.debug("redo log:$pos $rec")

      if (rec.transId > lastTrans) {
        lastTrans = rec.transId
      }
      readPos := store.logger.lastReadPos
      if (rec.transId != -1) {
        transSet[rec.transId] = pos
        pos = readPos
      } else {
        pos = readPos
        continue
      }

      switch (rec.type) {
        case PLogType.endCheckPoint:
          CheckLogRec ck := store.logger.readLog(pos)
          ck.transList.each |v, i|{
            transSet[v] = ck.transPos[i]
          }

        case PLogType.commitTrans:
          transSet.remove(rec.transId)
        case PLogType.abortTrans:
          transPos := transSet[rec.transId]
          undoFrom(transPos)
          transSet.remove(rec.transId)
        case PLogType.updatePage:
          PageLogRec prec := rec
          buf := prec.newData
          page := Page(store.pageSize, prec.pageId, buf.in)
          store.updatePageDirect(page)

        case PLogType.freePage:
          FreePageLogRec frec := rec
          store.freePage = frec.newFreePage
          store.pageCount = frec.newPageCount

        case PLogType.createPage:
          CreatePageLogRec frec := rec
          store.freePage = frec.newFreePage
          store.pageCount = frec.newPageCount
      }
    }

    store.lastTrans = lastTrans

    transSet.each |transPos,transId| {
      log.debug("undo uncompleted trans: $transId from: $transPos")
      undoFrom(transPos)
      p := store.logger.abortTransOffline(transId, transPos)
    }
  }

}