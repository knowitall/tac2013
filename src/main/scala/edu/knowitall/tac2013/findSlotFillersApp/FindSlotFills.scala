
package edu.knowitall.tac2013.findSlotFillersApp

import KBPSlotOpenIERelationTranslator.getOrganizationMap
import KBPSlotOpenIERelationTranslator.getPersonMap
import QueryEntityForAllSlots.executeEntityQueryForAllSlots
import KbpQueryOutput.printPersonOutput


//Command line application object for running solr queries on all the slots
//of a given entity and semantic type
object FindSlotFills {
  
  
  def main(args: Array[String]){

     assert(args.length == 3, 
         "there should be three arguments: entity name, semantic type (organization or person), and file path for output")
         
     val entityName = args(0).replace("_", " ")
     val semanticType = args(1)
     println(entityName)
     println(semanticType)
     
     if (semanticType == "organization"){
	     val orgMap = getOrganizationMap()
	     val arrayOfResults = executeEntityQueryForAllSlots(entityName, orgMap.toMap)
	     printPersonOutput(arrayOfResults,args(2))

  	 }
     
     else if (semanticType == "person"){
	     val perMap = getPersonMap()
	     val arrayOfResults = executeEntityQueryForAllSlots(entityName, perMap.toMap)
	     printPersonOutput(arrayOfResults,args(2))
     }
     
     else{
       
       throw new IllegalArgumentException("Second Argument must be either 'person' or 'organization'")
       
     }
     
     
       
    
  }

}