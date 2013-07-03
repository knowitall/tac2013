package edu.knowitall.tac2013.findSlotFillersApp

class QueryBuilder {

	var arg1String =""
	var arg2String =""
	var relString  =""
	var beginningOfArg2String = ""
	var arg1WikiLinkNodeIdString = ""
	var arg2WikiLinkNodeIdString = ""
	
	
	def getQueryString(): String = {
	  //make regex expression to fit the beginning of Arg2 String
	  var buildString = ""
	  if(arg1String != ""){
	    buildString += arg1String + " AND "
	  }
	  if(relString != ""){
	    buildString += relString + " AND "
	  }
	  if(arg2String != ""){
	    buildString += arg2String + " AND "
	  }
	  if(arg1WikiLinkNodeIdString != ""){
	    buildString += arg1WikiLinkNodeIdString + " AND "
	  }
	  if(arg2WikiLinkNodeIdString != ""){
	    buildString += arg2WikiLinkNodeIdString + " AND "
	  }
	  if(beginningOfArg2String != ""){
	    buildString += beginningOfArg2String
	  }
	  else{
	    buildString = buildString.substring(0, (buildString.length -5))
	  }
	  buildString

	}
	
	def setArg1String(arg1: String){
	  arg1String = "+arg1Text:\"" +arg1 + "\""
	}
	def setRelString(rel: String){
	  //for now we will erase any mention of <JobTitle> when specifying the query,
	  //and rely on the tagger semantic filter to choose extractions where a job title
	  //is present
	  val noJobTitle = rel.replace("<JobTitle>", "")
	  if(noJobTitle != ""){
	    relString = "+relText:\"" + noJobTitle + "\"" 
	  }
	}
	def setArg2String(arg2: String){
	  arg2String = "+arg2Text:\"" +arg2 + "\""
	}
	
	def setBeginningOfArg2String(beg: String){
	  beginningOfArg2String = "+arg2Text:\"" + beg + "\"" 
	}
	
	def setArg1WikiLinkNodeIdString(nodeId: String){
	  arg1WikiLinkNodeIdString = "+arg1WikiLinkNodeId:\"" + nodeId + "\""
	}
	
	def setArg2WikiLinkNodeIdString(nodeId: String){
	  arg2WikiLinkNodeIdString = "+arg2WikiLinkNodeId:\"" + nodeId + "\""
	}
	
	def buildQuery(relationData: KbpSlotToOpenIEData, queryEntity: String){
	  
	  	    val entityIn = relationData.entityIn.getOrElse({""})
	        if (entityIn.trim() == "arg1"){
	           setArg1String(queryEntity)
	        }
	        else if (entityIn.trim() == "arg2"){
	           setArg2String(queryEntity)   
	        }
	        else{
	           throw new Exception("entityIn contains invalid string")
	        }
	        setRelString(relationData.openIERelationString.getOrElse({""}))
	        val beginningOfArg2 = relationData.arg2Begins.getOrElse({""})
	        if (beginningOfArg2 != ""){
	          setBeginningOfArg2String(relationData.arg2Begins.get)
	        }	  
	}
	
	def buildLinkedQuery(relationData: KbpSlotToOpenIEData, nodeID: String){
	  
	  	    val entityIn = relationData.entityIn.getOrElse({""})
	        if (entityIn.trim() == "arg1"){
	           setArg1WikiLinkNodeIdString(nodeID)
	        }
	        else if (entityIn.trim() == "arg2"){
	           setArg2WikiLinkNodeIdString(nodeID)   
	        }
	        else{
	           throw new Exception("entityIn contains invalid string")
	        }
	        setRelString(relationData.openIERelationString.getOrElse({""}))
	        val beginningOfArg2 = relationData.arg2Begins.getOrElse({""})
	        if (beginningOfArg2 != ""){
	          setBeginningOfArg2String(relationData.arg2Begins.get)
	        }
	}
	
	
}