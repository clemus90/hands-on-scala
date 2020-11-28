def sync(src: os.Path, dest: os.Path) = {
    for (srcSubPath <- os.walk(src)) {
        val subPath = srcSubPath.subRelativeTo(src)
        val destSubPath = dest / subPath
        (os.isDir(srcSubPath), os.isDir(destSubPath)) match {
            case(false, true) | (true, false) =>
                os.copy.over(srcSubPath, destSubPath, createFolders = true)
            case (false, false)
                if !os.exists(destSubPath) 
                || !os.read.bytes(srcSubPath).sameElements(os.read.bytes(destSubPath)) =>
                os.copy.over(srcSubPath, destSubPath, createFolders = false)
            case _ => // do nothing
        }
    }

    for (dstSubPath <- os.walk(dest)) {
        val subPath = dstSubPath.subRelativeTo(dest)
        val srcSubPath = src / subPath
        (os.isDir(dstSubPath), os.isDir(srcSubPath)) match {
            case (true, false) => os.remove(dstSubPath)
            case (false, false)
                if os.exists(dstSubPath) && !os.exists(srcSubPath) => os.remove(dstSubPath)
            case _ => // do nothing

        }
    }
}