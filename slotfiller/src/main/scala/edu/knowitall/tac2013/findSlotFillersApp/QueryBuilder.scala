package edu.knowitall.tac2013.findSlotFillersApp

class QueryBuilder {

	var arg1String =""
	var arg2String =""
	var relString  =""
	var beginningOfArg2String = ""
	
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
	  relString = "+relText:\"" + noJobTitle + "\""
	}
	def setArg2String(arg2: String){
	  arg2String = "+arg2Text:\"" +arg2 + "\""
	}
	
	def setBeginningOfArg2String(beg: String){
	  beginningOfArg2String = "+arg2Text:\"" + beg + "\"" 
	}
}