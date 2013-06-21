package edu.knowitall.tac2013.findSlotFillersApp

import scala.io._
import java.io._

object KbpQueryOutput {
  
  def printPersonOutput(arrayOfResults: Array[(String,KbpSlotToOpenIEData,Array[Map[String,Any]])], filePath: String){
    
    val writer = new PrintWriter(new File(filePath))
    var printedKbpSlotNames = Array[String]()
    for(result <- arrayOfResults){
      
      val kbpSlotName = result._1
      val kbpOpenIEData = result._2
      //only return up to 20 solr Results
      val solrResultsArray = result._3.slice(0,20)
      
      if (!printedKbpSlotNames.contains(kbpSlotName)){
        writer.write("KBP SLOT NAME: " + kbpSlotName + "\n")
        printedKbpSlotNames = printedKbpSlotNames :+ kbpSlotName
      }
      
      writer.write("\tQuery Data:\t"+ "RelationTerms: "+kbpOpenIEData.OpenIERelationString
          + "\t Arg2Begins: " + kbpOpenIEData.Arg2Begins + "\t Entity In: " +
          kbpOpenIEData.EntityIn + "\t SlotFill In: " + kbpOpenIEData.SlotFillIn +
          "\t Slot type: " + kbpOpenIEData.SlotType +"\n")
      
      writer.write("\tResults:\n")
      if (solrResultsArray.length ==0){
        writer.write("\t\tNil" + "\n")
      }
      
      
      for (solrResult <- solrResultsArray){
        
        writer.write("\t\targ1: " + solrResult("arg1") + "\t rel: " + solrResult("rel") + 
            "\t arg2: " + solrResult("arg2") + "\t docID: " + solrResult("url") +
            "\t confidence: " + solrResult("confidence") + "\t sentence: " + solrResult("sentence") + "\n\n")


      }

      
    }
    
    writer.close()
    
  }

}