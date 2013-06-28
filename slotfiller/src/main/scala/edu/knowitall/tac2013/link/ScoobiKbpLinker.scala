package edu.knowitall.tac2013.link

import com.nicta.scoobi.Scoobi._
import scopt.OptionParser
import com.nicta.scoobi.io.text.TextOutput
import com.nicta.scoobi.io.text.TextInput
import com.nicta.scoobi.io.text.TextSource

import edu.knowitall.tac2013.openie.KbpExtraction

import java.io.File

object ScoobiKbpLinker extends ScoobiApp {

  val baseDir = "/scratch2"
  val mapFile = baseDir + "/wikimap.txt"
  
  def run(): Unit = {

    var inputPath, outputPath = ""

    val parser = new OptionParser() {
      arg("inputPath", "hdfs input path, sentences each on a line", { str => inputPath = str })
      arg("outputPath", "hdfs output path, chunked sentences", { str => outputPath = str })
    }

    if (!parser.parse(args)) return

    lazy val linker = KbpExtractionLinker.getKbpLinker(new File(baseDir), new File(mapFile))
    
    // serialized KbpExtractions
    val lines: DList[String] = TextInput.fromTextFile(inputPath)
    
    val linked = lines.mapFlatten { line => 
      KbpExtraction.read(line) map { extr =>
        linker.linkExtraction(extr)
      } map KbpExtraction.write
    }
    
    TextOutput.toTextFile(linked, outputPath).persist
  }
}