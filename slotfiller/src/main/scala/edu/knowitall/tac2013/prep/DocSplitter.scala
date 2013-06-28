package edu.knowitall.tac2013.prep

import java.io.File
import edu.knowitall.common.Resource
import java.util.LinkedList
import java.util.regex.Pattern
import scala.collection.JavaConverters._

/**
 * Reads KBP corpus and provides an iterator over
 * document elements (<DOC>) in the corpus.
 */
object DocSplitter {
  
  import java.io.PrintStream
  
  private val docCloseTag = Pattern.compile("\\s*(</DOC>|</doc>)\\s*")
  
  def apply(lines: Iterator[String]) = splitDocs(lines)
  
  def splitDocs(lines: Iterator[String]) = new Iterator[KbpRawDoc] {
    
    def hasNext = lines.hasNext
    
    // getNextDoc may not be thread safe
    def next = this.synchronized { getNextDoc(lines) }
  }
  
  private def getNextDoc(lines: Iterator[String]): KbpRawDoc = {
    
    val lineBuffer = new LinkedList[KbpDocLine]()
    
    var offset = 0
    
    var done = false
    
    while (!done && lines.hasNext) {
      
      val nextLine = lines.next() + "\n"
      if (docCloseTag.matcher(nextLine).matches()) done = true
      lineBuffer.add(new KbpDocLine(nextLine, offset))
      offset = offset + nextLine.length
    }
    new KbpRawDoc(lineBuffer.asScala.toList)
  }
  
  
  
  def main(args: Array[String]) {
    
    val inputFile = args(0)
    val outputDir = args(1)
    val docsToSplitStr = args(2)
    
    val docsToSplit = if (docsToSplitStr.equals("-1")) Int.MaxValue else docsToSplitStr.toInt
    
    var numDocs = 0
    
    val source = io.Source.fromFile(inputFile, "UTF8")	
    val docSpliterator = splitDocs(source.getLines).take(docsToSplit)
    
    docSpliterator.foreach { kbpDoc =>
      val output = new PrintStream("%s/doc%d.txt".format(outputDir, numDocs), "UTF8")
      kbpDoc.lines.foreach { kbpLine => output.print(kbpLine.line) }
      numDocs += 1
    }
    
    source.close()
  }
  
  
}