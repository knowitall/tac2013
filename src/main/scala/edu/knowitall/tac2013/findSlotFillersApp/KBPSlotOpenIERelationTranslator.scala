
package edu.knowitall.tac2013.findSlotFillersApp
import scala.io.Source
import scala.collection.mutable.Map

object KBPSlotOpenIERelationTranslator {
  
  def getOrganizationMap(): Map[String,List[KbpSlotToOpenIEData]] = {
    val source = Source.fromURL(getClass.getResource("/edu/knowitall/tac2013/findSlotFillersApp/KBP-OpenIE1.csv"))
    val lines = source.getLines().toList
    var KBP_OpenIEMap = Map[String,List[KbpSlotToOpenIEData]]()
    for( line <- lines){
      println(line)
      val splitLine = line.split(",")
      if (splitLine.length == 7){
      if (splitLine(0).contains("org:")){
		    var csvDataArray = Array[String]()
		    for (sl <- splitLine){
		        csvDataArray = (csvDataArray :+ sl)
		     }
		    if (splitLine.length ==7){
		      val kbpOpenIEData = KbpSlotToOpenIEData(csvDataArray(0),csvDataArray(1),
		          csvDataArray(2),csvDataArray(3),csvDataArray(4),csvDataArray(5),csvDataArray(6))
		      if (KBP_OpenIEMap contains csvDataArray(0)){
		         val KBP_OpenIEMapValueList = KBP_OpenIEMap(csvDataArray(0))
		         val KBP_OpenIEMapValueListPlusOneElement = kbpOpenIEData :: KBP_OpenIEMapValueList
		         KBP_OpenIEMap += (csvDataArray(0) -> KBP_OpenIEMapValueListPlusOneElement)
		      }
		      else{
		         val KBP_OpenIEMapValueList = List(kbpOpenIEData)
		         KBP_OpenIEMap += (csvDataArray(0) -> KBP_OpenIEMapValueList)
		      }
		    }
		  }
		}
    }
    KBP_OpenIEMap
  }
    
  
  def getPersonMap(): Map[String,List[KbpSlotToOpenIEData]] = {
    val source = Source.fromURL(getClass.getResource("/edu/knowitall/tac2013/findSlotFillersApp/KBP-OpenIE1.csv"))
    val lines = source.getLines().toList
    var KBP_OpenIEMap = Map[String,List[KbpSlotToOpenIEData]]()
    for( line <- lines){
      println(line)
      val splitLine = line.split(",")
      if (splitLine.length == 7){
      if (splitLine(0).contains("per:")){
		    var csvDataArray = Array[String]()
		    for (sl <- splitLine){
		        csvDataArray = (csvDataArray :+ sl)
		     }
		    if (splitLine.length ==7){
		      val kbpOpenIEData = KbpSlotToOpenIEData(csvDataArray(0),csvDataArray(1),
		          csvDataArray(2),csvDataArray(3),csvDataArray(4),csvDataArray(5),csvDataArray(6))
		      if (KBP_OpenIEMap contains csvDataArray(0)){
		         val KBP_OpenIEMapValueList = KBP_OpenIEMap(csvDataArray(0))
		         val KBP_OpenIEMapValueListPlusOneElement = kbpOpenIEData :: KBP_OpenIEMapValueList
		         KBP_OpenIEMap += (csvDataArray(0) -> KBP_OpenIEMapValueListPlusOneElement)
		      }
		      else{
		         val KBP_OpenIEMapValueList = List(kbpOpenIEData)
		         KBP_OpenIEMap += (csvDataArray(0) -> KBP_OpenIEMapValueList)
		      }
		    }
		  }
		}
    }
    KBP_OpenIEMap
  }

}