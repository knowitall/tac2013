package edu.knowitall.tac2013.findSlotFillersApp

object SlotFillConsistency {
  
  
  def makeConsistent(answers: Map[Slot,Seq[Candidate]]): Map[Slot,Seq[Candidate]] = {
    
    makeLocationsConsistent(answers)
  }
  
  def makeLocationsConsistent(answers: Map[Slot, Seq[Candidate]]): Map[Slot,Seq[Candidate]] = {
    
    var countrySlot : Option[Slot] = None
    var citySlot : Option[Slot] = None
    var stateOrProvinceSlot : Option[Slot] = None
    
    var countryDeathSlot : Option[Slot] = None
    var cityDeathSlot : Option[Slot] = None
    var stateOrProvinceDeathSlot : Option[Slot] = None
    
    
    var countryListSlot : Option[Slot] = None
    var cityListSlot : Option[Slot] = None
    var stateOrProvinceListSlot : Option[Slot] = None
    
    var locationConsistentMap = scala.collection.mutable.Map[Slot,Seq[Candidate]]()
    
    // get all the slot keys for accessing best answers
    for(slot <- answers.keys){
      locationConsistentMap += (slot -> answers(slot))
      if(slot.isLocation){
        if(slot.isCity){
          if(slot.name.contains("death")){
            cityDeathSlot = Some(slot)
          }
          else{
          citySlot = Some(slot)
          }
        }
        if(slot.isCountry){
          if(slot.name.contains("death")){
            countryDeathSlot = Some(slot)
          }
          else{
          countrySlot = Some(slot)
          }
        }
        if(slot.isStateOrProvince){
          if(slot.name.contains("death")){
            stateOrProvinceDeathSlot = Some(slot)
          }
          else{ 
           stateOrProvinceSlot = Some(slot)
          }
        }
        if(slot.isCityList){
          cityListSlot = Some(slot)
        }
        if(slot.isStateOrProvinceList){
          stateOrProvinceListSlot = Some(slot)
        }
        if(slot.isCountryList){
          countryListSlot = Some(slot)
        }
      }
    }
    
    
    //ensure that if there is only one instance of the slot fill string in the justification
    //that city, country, and state or province should not be the same
    if(countrySlot.isDefined && stateOrProvinceSlot.isDefined){
    	if(secondLocationSlotFillCopiesFirstLocationSlotFill(countrySlot.get,stateOrProvinceSlot.get,answers)){
             //replace list with empty list for stateOrProvinceSlot
    	     
             locationConsistentMap += (breakTie(countrySlot.get,stateOrProvinceSlot.get,answers) -> Seq[Candidate]())
    	}
    }
    if(countrySlot.isDefined && citySlot.isDefined){
    	if(secondLocationSlotFillCopiesFirstLocationSlotFill(countrySlot.get,citySlot.get,answers)){
             //replace list with empty list for stateOrProvinceSlot
             locationConsistentMap += (breakTie(countrySlot.get,citySlot.get,answers) -> Seq[Candidate]())
    	}
    }
    if(stateOrProvinceSlot.isDefined && citySlot.isDefined){
    	if(secondLocationSlotFillCopiesFirstLocationSlotFill(stateOrProvinceSlot.get,citySlot.get,answers)){
             //replace list with empty list for stateOrProvinceSlot
             locationConsistentMap += (breakTie(stateOrProvinceSlot.get,citySlot.get,answers) -> Seq[Candidate]())
    	}
    }
    
    if(countryDeathSlot.isDefined && stateOrProvinceDeathSlot.isDefined){
    	if(secondLocationSlotFillCopiesFirstLocationSlotFill(countryDeathSlot.get,stateOrProvinceDeathSlot.get,answers)){
             //replace list with empty list for stateOrProvinceSlot
             locationConsistentMap += (breakTie(countryDeathSlot.get,stateOrProvinceDeathSlot.get,answers) -> Seq[Candidate]())
    	}
    }
    if(countryDeathSlot.isDefined && cityDeathSlot.isDefined){
    	if(secondLocationSlotFillCopiesFirstLocationSlotFill(countryDeathSlot.get,cityDeathSlot.get,answers)){
             //replace list with empty list for stateOrProvinceSlot
             locationConsistentMap += (breakTie(countryDeathSlot.get,cityDeathSlot.get,answers) -> Seq[Candidate]())
    	}
    }
    if(stateOrProvinceDeathSlot.isDefined && cityDeathSlot.isDefined){
    	if(secondLocationSlotFillCopiesFirstLocationSlotFill(stateOrProvinceDeathSlot.get,cityDeathSlot.get,answers)){
             //replace list with empty list for stateOrProvinceSlot
             locationConsistentMap += (breakTie(stateOrProvinceDeathSlot.get,cityDeathSlot.get,answers) -> Seq[Candidate]())
    	}
    }
    
    if(countryListSlot.isDefined && stateOrProvinceListSlot.isDefined){
         val truncatedLists = getTruncatedListForSlotFills(countryListSlot.get,stateOrProvinceListSlot.get,locationConsistentMap.toMap)
         locationConsistentMap += (countryListSlot.get -> truncatedLists(0))
         locationConsistentMap += (stateOrProvinceListSlot.get -> truncatedLists(1))
    }
    
    if(countryListSlot.isDefined && cityListSlot.isDefined){
         val truncatedLists = getTruncatedListForSlotFills(countryListSlot.get,cityListSlot.get,locationConsistentMap.toMap)
         locationConsistentMap += (countryListSlot.get -> truncatedLists(0))
         locationConsistentMap += (cityListSlot.get -> truncatedLists(1))
    }
    if(stateOrProvinceListSlot.isDefined && cityListSlot.isDefined){
         val truncatedLists = getTruncatedListForSlotFills(countryListSlot.get,cityListSlot.get,locationConsistentMap.toMap)
         locationConsistentMap += (stateOrProvinceListSlot.get -> truncatedLists(0))
         locationConsistentMap += (cityListSlot.get -> truncatedLists(1))
    }
        
 
   
    
    locationConsistentMap.toMap
  }
  
  /**
   * Pass in two location slots and their map to best answers. This will return true if the second slot's best answer
   * is a copy of the first slots best answer when there should only be one instance of the answer string.
   */
  def secondLocationSlotFillCopiesFirstLocationSlotFill(slot1: Slot, slot2: Slot, answers: Map[Slot, Seq[Candidate]]): Boolean = {
    require((slot1.isLocation && slot2.isLocation), {println("To use this method slots must be of location type")})
    
    val slot1BestAnswer = answers(slot1).headOption
    val slot2BestAnswer = answers(slot2).headOption
    var doRemove = false
    
    if(slot1BestAnswer.isDefined && slot2BestAnswer.isDefined){
      val slot1Answer = slot1BestAnswer.get
      val slot2Answer = slot2BestAnswer.get
      val slot2Extr = slot2Answer.extr
      val sent = slot2Extr.sentenceText

    
      val slot1AnswerString = slot1Answer.trimmedFill.string
      val slot2AnswerString = slot2Answer.trimmedFill.string
    
      if(slot1AnswerString == slot2AnswerString){
         doRemove = true
         val firstOccurrence = sent.indexOfSlice(slot1AnswerString)
         if(firstOccurrence != -1){
           val secondOccurrence = sent.slice(firstOccurrence + slot1AnswerString.length(), sent.length()).indexOfSlice(slot1AnswerString)
           if(secondOccurrence != -1){
              doRemove = false
           }
         }
      }
    }
      
      
    doRemove
  }
  
  
    /**
   * Pass in two location list slots and their map to best answers. This will return a list of 2 lists of
   * the new resulst after collisions have been resolved.
   */
  def getTruncatedListForSlotFills(slot1: Slot, slot2: Slot, answers: Map[Slot, Seq[Candidate]]): Seq[Seq[Candidate]] = {
    require((slot1.isLocation && slot2.isLocation && slot1.isList && slot2.isList), {println("To use this method slots must be of location type and list type")})
    
    val slot1BestAnswers = answers(slot1)
    val slot2BestAnswers = answers(slot2)
    var doRemove = false
    
    var truncatedSlot1Array = scala.collection.mutable.ArrayBuffer[Candidate]()
    for(ans <- slot1BestAnswers){
      truncatedSlot1Array ++= List(ans)
    }
    
    var truncatedSlot2Array = scala.collection.mutable.ArrayBuffer[Candidate]()
    for(ans <- slot2BestAnswers){
      truncatedSlot2Array ++= List(ans)
    }
    
    
    
    
    // if both lists are not empty then there may be some collissions
    if(!slot1BestAnswers.isEmpty && !slot2BestAnswers.isEmpty){
      
      //iterate through all answers for slot 1 and check that slot 2 is not copying them
      for(slot1Answer <- slot1BestAnswers){
        val slot1AnswerString = slot1Answer.trimmedFill.string
        
        for(slot2Answer <- slot2BestAnswers){
          val slot2AnswerString = slot2Answer.trimmedFill.string
          val slot2Extr = slot2Answer.extr
          val sent  = slot2Extr.sentenceText
          doRemove = false
	      if(slot1AnswerString == slot2AnswerString){
	         doRemove = true
	         val firstOccurrence = sent.indexOfSlice(slot1AnswerString)
	         if(firstOccurrence != -1){
	           val secondOccurrence = sent.slice(firstOccurrence + slot1AnswerString.length(), sent.length()).indexOfSlice(slot1AnswerString)
	           if(secondOccurrence != -1){
	              doRemove = false
	           }
	         }
	      }
          if(doRemove){
            //break a Tie to see which list the location belongs in
            var worstSlot = breakTieGivenString(slot1,slot2,slot1AnswerString)
            val truncatedList = worstSlot match{
              case `slot1` => {truncatedSlot1Array}
              case `slot2` => {truncatedSlot2Array}
            }
            //remove from truncated Array
            var ansToRemove: Option[Candidate] = None
            for(ans <- truncatedList){
              if(ans.trimmedFill.string == slot2AnswerString){
                ansToRemove = Some(ans)
              }
            }
            if(ansToRemove.isDefined){
              truncatedList -= ansToRemove.get
            }
            
          }
        }
      }
    }
    
    Seq(truncatedSlot1Array,truncatedSlot2Array)
  }
  
  /**
   * return the slot that is the least likely according to Nell to be the right slot
   */
  def breakTie(slot1: Slot, slot2: Slot, answers: Map[Slot, Seq[Candidate]]): Slot = {
    val slot1Prob = getNellProb(answers(slot1).head.trimmedFill.string,slot1)
    val slot2Prob = getNellProb(answers(slot2).head.trimmedFill.string,slot2)
    
    if(slot1Prob == slot2Prob){
      slot2
    }
    else if(slot1Prob > slot2Prob){
      slot2
    }
    else{
      slot1
    }
    
  }
  
  /**
   * returns least likely slot
   */
  def breakTieGivenString(slot1: Slot, slot2: Slot, str:String): Slot = {
    val slot1Prob = getNellProb(str,slot1)
    val slot2Prob = getNellProb(str,slot2)
    
    if(slot1Prob == slot2Prob){
      slot2
    }
    else if(slot1Prob > slot2Prob){
      slot2
    }
    else{
      slot1
    }
  }
  
  def getNellProb(str: String, slot: Slot): Double ={
    val nellData = NellData.getNellData(str.toLowerCase().trim())
    if(nellData.isDefined){
      if(slot.isCity || slot.isCityList){
        nellData.get.cityProbability.getOrElse(0.0)
      }
      else if(slot.isCountry || slot.isCountryList){
        nellData.get.countryProbability.getOrElse(0.0)
      }
      else if(slot.isStateOrProvince || slot.isStateOrProvinceList){
        nellData.get.stateOrProvinceProbability.getOrElse(0.0)
      }
      else{
        0.0
      }
    }
    else{
      0.0
    }
  } 
}