package edu.knowitall.tac2013.findSlotFillersApp

import QueryEntityForAllSlots.executeEntityQueryForAllSlots
import KBPSlotOpenIERelationTranslator.getOrganizationMap
import KBPSlotOpenIERelationTranslator.getPersonMap
import KbpQueryOutput.printFormattedOutputForKBPQuery
import java.io._

object KBPQueryExecutor {
  

  def executeKbpQuery(kbpQuery: KBPQuery, outputPath: String){
    kbpQuery.entityType match {
      case KBPQueryEntityType.ORG => {
        val orgMap = getOrganizationMap()
        
        //filter out ignored slots
        var filteredOrgMap = Map[String,List[KbpSlotToOpenIEData]]()
        for(slot <- kbpQuery.slotsToFill){
          filteredOrgMap += (slot -> orgMap(slot))
        }
        val results = executeEntityQueryForAllSlots(kbpQuery.name,filteredOrgMap)
         printFormattedOutputForKBPQuery(results,outputPath,kbpQuery)       
      }
      case KBPQueryEntityType.PER => {
        val perMap = getPersonMap()
        
        //filter out ignored slots
        var filteredPerMap = Map[String,List[KbpSlotToOpenIEData]]()
        for(slot <- kbpQuery.slotsToFill){
          filteredPerMap += (slot -> perMap(slot))
        }
        val results = executeEntityQueryForAllSlots(kbpQuery.name,filteredPerMap)
        printFormattedOutputForKBPQuery(results,outputPath,kbpQuery)
      }
      case _ => throw new Exception("Entity Type must be person or organization")
    }
  }
  
  def executeKbpQueries(kbpQueryList: List[KBPQuery], outputPath:String){
     
    
     //delete output file before appending to it
     val f =  new File(outputPath)
     if(f.exists()){
       f.delete()
     }
    
    for(kbpQuery <- kbpQueryList){
      executeKbpQuery(kbpQuery,outputPath)
    }
  }
  
    
  def main(args: Array[String]){

     assert(args.length == 2, 
         "there should be two arguments: path to KBP query File, path to the output File")
         
     val KBPQueryPath = args(0)
     val outputPath = args(1)
     
     
     val kbpQueryList = KBPQuery.parseKBPQueries(KBPQueryPath)
     executeKbpQueries(kbpQueryList,outputPath)    
  }
         
     
}