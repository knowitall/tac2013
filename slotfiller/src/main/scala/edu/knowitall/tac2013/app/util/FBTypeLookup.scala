package edu.knowitall.tac2013.app.util

import edu.knowitall.browser.entity.EntityTyper
import edu.knowitall.tac2013.openie.WikiLink
import edu.knowitall.tac2013.openie.KbpExtractionField
import java.io.File

object FBTypeLookup {

  val basePaths = Seq("/scratch/", "/scratch2/").map(new File(_))
  
  val baseFileLookup = basePaths.find(_.exists())
  
  lazy val typer = baseFileLookup match {
    case Some(baseFile) => Some(new EntityTyper(baseFile))
    case None => None
  }
  
  def getFbTypes(fbid: String): Iterable[String] = typer match {
    case None => { System.err.println("Unable to load Freebase type lookup table."); Nil }
    case Some(typer) => typer.typeFbid(fbid)
  }
  
  def getFbTypes(wikiLink: WikiLink): Iterable[String] = getFbTypes(wikiLink.fbid)
  
  def getFbTypes(field: KbpExtractionField): Iterable[String] = field.wikiLink.toSeq.flatMap(getFbTypes)
  
  def main(args: Array[String]): Unit = {
    println(getFbTypes("04sv4").mkString("\n"))
  }
}

