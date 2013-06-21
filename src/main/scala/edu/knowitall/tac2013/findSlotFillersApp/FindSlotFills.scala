
package edu.knowitall.tac2013.findSlotFillersApp

import KBPSlotOpenIERelationTranslator.getOrganizationMap
import KBPSlotOpenIERelationTranslator.getPersonMap
import QueryEntityForAllSlots.executeEntityQueryForAllSlots
import KbpQueryOutput.printPersonOutput

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
	     executeEntityQueryForAllSlots(entityName, orgMap.toMap)
  	 }
     else if (semanticType == "person"){
	     val perMap = getPersonMap()
	     val arrayOfResults = executeEntityQueryForAllSlots(entityName, perMap.toMap)
	     printPersonOutput(arrayOfResults,args(2))
     }
     
     else{
       
     }
     
     
       
    
  }

}