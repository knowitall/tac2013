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
	      //only look at leftmost tag
	      val firstTag = locationTypesInSlotFill.head
	      if (isCity(firstTag.text())){
	        return Some(firstTag)
	      }
	      return None
	    }
	    else if(slotType=="Country"){
	      //only look at right most tag
	      val lastTag = locationTypesInSlotFill(locationTypesInSlotFill.length-1)
	      if(isCountry(lastTag.text())){
	        return Some(lastTag)
	      }
	      return None
	    }
	    else if(slotType=="Stateorprovince"){
	      //if more than 2 tags look at 2nd tag only otherwise look at the only tag
	      var tag = locationTypesInSlotFill.head
	      if(locationTypesInSlotFill.length >1){
	        tag = locationTypesInSlotFill(1)
	      }
	      
	      if(isStateOrProvince(tag.text())){
	        return Some(tag)
	      }

	      return None
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
    if (TipsterData.stateOrProvinces.contains(str.toLowerCase()) || (NellData.stateOrProvinceNameSet.contains(str.toLowerCase()))) true else false
  }
  
  def isCountry(str: String): Boolean = {
    if (TipsterData.countries.contains(str.toLowerCase()) || (NellData.countryNameSet.contains(str.toLowerCase()))) true else false
  }

}