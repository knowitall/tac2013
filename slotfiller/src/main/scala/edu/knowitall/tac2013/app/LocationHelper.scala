package edu.knowitall.tac2013.app

import edu.knowitall.taggers.Type
import edu.knowitall.collection.immutable.Interval


object LocationHelper {
  
  /**
   * findLocationTaggedType returns the Type that should make up the trimmed
   * slot fill for a given location type specification. If the method returns None it will be assumed
   *  that this extraction should be filtered out and this method should be used in the semantic
   *   filter in FilterSolrREsults.scala
   */
  def findLocationTaggedType(typesInSlotFill : List[Type], slotType: String): Option[Type] = {
    val locationTypesInSlotFill = typesInSlotFill.filter(t => (t.descriptor() == "StanfordLOCATION"))
    if(locationTypesInSlotFill.nonEmpty){
      
      
	    if(slotType == "City"){
	      val firstTag = locationTypesInSlotFill.head
	      //if there is only one tag, then if the name is in NELL it should only 
	      //be accepted as a city if city has the highest probability
	      if(locationTypesInSlotFill.length  == 1){
	        val nellData = NellData.getNellData(firstTag.text())
	        if(nellData.isDefined){
		        if(nellData.get.highestProbabilityIsCity && isCity(firstTag.text())){
		          return Some(firstTag)
		        }
		        else{
		          return None
		        }
		      }
	         else{
	            if (isCity(firstTag.text())){
		          return Some(firstTag)
		        }
		        return None
	         }
	      }
	      //else if more than one tag only look at the left most tag and accept it
	      //as a city if it appears anywhere as a city
	      else{
	           //only look at leftmost tag
		      if (isCity(firstTag.text())){
		        return Some(firstTag)
		      }
		      return None
	      }
	    }
	    
	    
	    else if(slotType=="Country"){
	      //only look at right most tag
	      val lastTag = locationTypesInSlotFill(locationTypesInSlotFill.length-1)
	      
	      if(locationTypesInSlotFill.length ==1){
	        val nellData = NellData.getNellData(lastTag.text())
	        if(nellData.isDefined){
	          if(nellData.get.highestProbabilityIsCountry && isCountry(lastTag.text())){
	            return Some(lastTag)
	          }
	          else{
	            return None
	          }
	        }
	        else{
		      if(isCountry(lastTag.text())){
		        return Some(lastTag)
		      }
		      return None
	        }
	      }
	      else{
		      if(isCountry(lastTag.text())){
		        return Some(lastTag)
		      }
		      return None
	      }
	    }
	    
	    
	    
	    
	    
	    else if(slotType=="Stateorprovince"){
	      //if more than 2 tags look at 2nd tag only otherwise look at the only tag
	      var tag = locationTypesInSlotFill.head
	      if(locationTypesInSlotFill.length >1){
	        tag = locationTypesInSlotFill(1)
	      }
	      
	      
	      if(locationTypesInSlotFill.length ==1){
	        val nellData = NellData.getNellData(tag.text())
	        if(nellData.isDefined){
	          if(nellData.get.highestProbabilityIsStateOrProvince && isStateOrProvince(tag.text())){
	            return Some(tag)
	          }
	          else{
	            return None
	          }
	        }
	        else{
		      if(isStateOrProvince(tag.text())){
		        return Some(tag)
		      }
		      else{
		    	  return None
		      }	          
	        }
	      }
	      else{
		      if(isStateOrProvince(tag.text())){
		        return Some(tag)
		      }
		      else{
		    	  return None
		      }
	      }
	    }
	    else{
	      throw new Exception("There should only be three types of locations specified in the pattern, Country, Stateorprovince, and City")
	    }
    }
    else{None }
    
  }
  
  /**
   * Uses multiple databases to return a boolean if the string appears in any of these databases
   */
  def isCity(str: String): Boolean = {
    if (TipsterData.cities.contains(str.toLowerCase()) || (NellData.cityNameSet.contains(str.toLowerCase()))) true else false
  }
  
  def isStateOrProvince(str: String): Boolean = {
    if ( (TipsterData.stateOrProvinces.contains(str.toLowerCase()) ||
        (NellData.stateOrProvinceNameSet.contains(str.toLowerCase()))
         && 
         (!str.toLowerCase().contains("city")))) true else false
  }
  
  def isCountry(str: String): Boolean = {
    if ( (TipsterData.countries.contains(str.toLowerCase()) || 
        (NellData.countryNameSet.contains(str.toLowerCase()))     
         &&
         (!str.toLowerCase().contains("city")))) true else false
  }

}