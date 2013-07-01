package edu.knowitall.tac2013.prep.util

import org.scalatest._
import java.io.ByteArrayInputStream

class LineReaderSpec extends FlatSpec {

  "A LineReader" should "correctly return lines with line terminators" in {
    
    val testLines = Seq(
      Line("This is the first line.", "\n"),
      Line("This is the second line.", "\r"),
      Line("The next 2 lines are blank.", "\r\n"),
      Line("", "\r"),
      Line("", "\r\n"),
      Line("This is actually two line terminators:", "\n\r"),
      Line("This is the last line.", ""))
    
    val rawText = testLines.map { case Line(text, end) => text + end } mkString
    
    val expected = List(
      Line("This is the first line.", "\n"),
      Line("This is the second line.", "\r"),
      Line("The next 2 lines are blank.", "\r\n"),
      Line("", "\r"),
      Line("", "\r\n"),
      Line("This is actually two line terminators:", "\n"),
      Line("", "\r"),
      Line("This is the last line.", ""))
    
    val bstream = new ByteArrayInputStream(rawText.getBytes("UTF8"))
    val lineReader = LineReader.fromInputStream(bstream, "UTF8") 
    
    val result = lineReader.toList
    assert(result === expected)  
  }
}