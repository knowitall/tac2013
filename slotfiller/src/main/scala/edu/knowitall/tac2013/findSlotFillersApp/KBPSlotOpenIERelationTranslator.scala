
package edu.knowitall.tac2013.findSlotFillersApp
import scala.io.Source
import scala.collection.mutable.Map

object KBPSlotOpenIERelationTranslator {
  
  //returns a Map from the String of each KBPSlot to a list of all the specified query information
  def getOrganizationMap(): Map[String,List[KbpSlotToOpenIEData]] = {
    val source = Source.fromURL(getClass.getResource("/edu/knowitall/tac2013/findSlotFillersApp/KBP-OpenIE-Organization.csv"))
    val lines = source.getLines().toList
    var KBP_OpenIEMap = Map[String,List[KbpSlotToOpenIEData]]()
    var lastMaxValue = -1
    for( line <- lines){
     //println(line)
      val lineWithSpaces = line.replace(",", " , ")
      val splitLine = lineWithSpaces.split(",")
      if (lineWithSpaces.contains("org:")){
	      if (splitLine(0).contains("org:")){
			      var csvDataArray = Array[String]()
			      for (sl <- splitLine){
			          csvDataArray = (csvDataArray :+ sl)
			       }
			      
			      var maxValue = ""
			      //handle integer representing maxValue in csv file
			      //when integer is not in file you can assume it is the
			      //same as the last integer in this tab
			      if(!csvDataArray(1).trim().isEmpty()){
			        lastMaxValue = csvDataArray(1).trim().toInt
			        maxValue = lastMaxValue.toString
			      }
			      else if(lastMaxValue == -1){
			        maxValue = ""
			      }
			      else{
			        maxValue = lastMaxValue.toString
			      }
			      
			      val kbpOpenIEData = KbpSlotToOpenIEData.getKbpSlotToOpenIEDataInstance(csvDataArray(0).trim(),maxValue.trim(),
			          csvDataArray(2).trim(),csvDataArray(3).trim(),csvDataArray(4).trim(),csvDataArray(5).trim(),csvDataArray(6).trim())
			      
			      val kbpSlotName = csvDataArray(0).trim()    
			      
			      if (KBP_OpenIEMap contains kbpSlotName){
			         val KBP_OpenIEMapValueList = KBP_OpenIEMap(kbpSlotName)
			         val KBP_OpenIEMapValueListPlusOneElement = kbpOpenIEData :: KBP_OpenIEMapValueList
			         KBP_OpenIEMap += (kbpSlotName -> KBP_OpenIEMapValueListPlusOneElement)
			      }
			      else{
			         val KBP_OpenIEMapValueList = List(kbpOpenIEData)
			         KBP_OpenIEMap += (kbpSlotName -> KBP_OpenIEMapValueList)
			      }
			    } 
      }
    }
    source.close()
    KBP_OpenIEMap
  }
    
  
  def getPersonMap(): Map[String,List[KbpSlotToOpenIEData]] = {
    val source = Source.fromURL(getClass.getResource("/edu/knowitall/tac2013/findSlotFillersApp/KBP-OpenIE-Person.csv"))
    val lines = source.getLines().toList
    var KBP_OpenIEMap = Map[String,List[KbpSlotToOpenIEData]]()
    var lastMaxValue = -1
    for( line <- lines){
     //println(line)
      val lineWithSpaces = line.replace(",", " ,")
      val splitLine = lineWithSpaces.split(",")
      if (lineWithSpaces.contains("per:")){
	      if (splitLine(0).contains("per:")){
			      var csvDataArray = Array[String]()
			      for (sl <- splitLine){
			          csvDataArray = (csvDataArray :+ sl)
			       }
			      
			      var maxValue = ""
			      //handle integer representing maxValue in csv file
			      //when integer is not in file you can assume it is the
			      //same as the last integer in this tab
			      if (!csvDataArray(1).trim.isEmpty()){
			        lastMaxValue = csvDataArray(1).trim().toInt
			        maxValue = lastMaxValue.toString
			      }
			      else if(lastMaxValue == -1){
			        maxValue = ""
			      }
			      else{
			        maxValue = lastMaxValue.toString
			      }
			      
			      val kbpOpenIEData = KbpSlotToOpenIEData.getKbpSlotToOpenIEDataInstance(csvDataArray(0).trim(),maxValue.trim(),
			          csvDataArray(2).trim(),csvDataArray(3).trim(),csvDataArray(4).trim(),csvDataArray(5).trim(),csvDataArray(6).trim())
			      
			      val kbpSlotName = csvDataArray(0).trim()  
			          
			      if (KBP_OpenIEMap contains kbpSlotName){
			         val KBP_OpenIEMapValueList = KBP_OpenIEMap(kbpSlotName)
			         val KBP_OpenIEMapValueListPlusOneElement = kbpOpenIEData :: KBP_OpenIEMapValueList
			         KBP_OpenIEMap += (kbpSlotName -> KBP_OpenIEMapValueListPlusOneElement)
			      }
			      else{
			         val KBP_OpenIEMapValueList = List(kbpOpenIEData)
			         KBP_OpenIEMap += (kbpSlotName -> KBP_OpenIEMapValueList)
			      }
			    } 
      }
    }
    source.close()
    KBP_OpenIEMap
  }

}