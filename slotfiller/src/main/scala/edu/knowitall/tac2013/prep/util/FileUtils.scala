package edu.knowitall.tac2013.prep.util

import scala.io.Source
import java.io.File

/**
 * Some general helper methods pertaining to file i/o.
 */
object FileUtils {

  /**
   * If file is not a directory, return it
   * else, return all files (that are not directories) found recursively in this directory.
   */
  def getFilesRecursive(file: File): Iterator[File] = {
    if (!file.isDirectory()) Iterator(file)
    else (file.listFiles.iterator.flatMap { f => getFilesRecursive(f) })
  }

  /**
   *  Lazily flatten the lines from an iterator of sources while
   *  closing sources as they become empty.
   */
  def getLines(sources: Iterator[Source]): Iterator[String] = {
    val iter = new Iterator[Iterator[String]]() {
      def hasNext = sources.hasNext
      def next = new Iterator[String]() {
        val source = sources.next()
        var closed = false
        val lines = source.getLines
        def hasNext = { // lines.hasNext will fail if source is closed.
          if (closed)
            false
          else if (!lines.hasNext) {
            closed = true
            false
          } else
            true
        }
        def next() = {
          val line = lines.next
          if (!lines.hasNext) { source.close; closed = true }
          line
        }
      }
    }
    iter.flatten
  }
}