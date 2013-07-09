package edu.knowitall.tac2013.findSlotFillersApp

object SlotFillConsistency {
  
  
  def makeConsistent(answers: Map[Slot,Seq[Candidate]]): Map[Slot,Seq[Candidate]] = {
    
    makeLocationsConsistent(answers)
  }
  
  def makeLocationsConsistent(answers: Map[Slot, Seq[Candidate]]): Map[Slot,Seq[Candidate]] = {
    
    var countrySlot : Option[Slot] = None
    var citySlot : Option[Slot] = None
    var stateOrProvinceSlot : Option[Slot] = None
    
    var countryListSlot : Option[Slot] = None
    var cityListSlot : Option[Slot] = None
    var stateOrProvinceListSlot : Option[Slot] = None
    
    var locationConsistentMap = scala.collection.mutable.Map[Slot,Seq[Candidate]]()
    
    // get all the slot keys for accessing best answers
    for(slot <- answers.keys){
      locationConsistentMap += (slot -> answers(slot))
      if(slot.isLocation){
        if(slot.isCity){
          citySlot = Some(slot)
        }
        if(slot.isCountry){
          countrySlot = Some(slot)
        }
        if(slot.isStateOrProvince){
          stateOrProvinceSlot = Some(slot)
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
             locationConsistentMap += (stateOrProvinceSlot.get -> Seq[Candidate]())
    	}
    }
    if(countrySlot.isDefined && citySlot.isDefined){
    	if(secondLocationSlotFillCopiesFirstLocationSlotFill(countrySlot.get,citySlot.get,answers)){
             //replace list with empty list for stateOrProvinceSlot
             locationConsistentMap += (citySlot.get -> Seq[Candidate]())
    	}
    }
    if(stateOrProvinceSlot.isDefined && citySlot.isDefined){
    	if(secondLocationSlotFillCopiesFirstLocationSlotFill(stateOrProvinceSlot.get,citySlot.get,answers)){
             //replace list with empty list for stateOrProvinceSlot
             locationConsistentMap += (citySlot.get -> Seq[Candidate]())
    	}
    }
    
    if(countryListSlot.isDefined && stateOrProvinceListSlot.isDefined){
         locationConsistentMap += (stateOrProvinceListSlot.get -> getTruncatedListOfSlotFillsInSlot2ThatDoNotCopySlot1(countryListSlot.get,stateOrProvinceListSlot.get,answers))
    }
    
    if(countryListSlot.isDefined && cityListSlot.isDefined){
         locationConsistentMap += (cityListSlot.get -> getTruncatedListOfSlotFillsInSlot2ThatDoNotCopySlot1(countryListSlot.get,cityListSlot.get,answers))
    }
    if(stateOrProvinceListSlot.isDefined && cityListSlot.isDefined){
         locationConsistentMap += (cityListSlot.get -> getTruncatedListOfSlotFillsInSlot2ThatDoNotCopySlot1(stateOrProvinceListSlot.get,cityListSlot.get,answers))
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
   * Pass in two location list slots and their map to best answers. This will return the new list of best answers for the second slot
   * given that the first slot is trusted more.
   */
  def getTruncatedListOfSlotFillsInSlot2ThatDoNotCopySlot1(slot1: Slot, slot2: Slot, answers: Map[Slot, Seq[Candidate]]): Seq[Candidate] = {
    require((slot1.isLocation && slot2.isLocation && slot1.isList && slot2.isList), {println("To use this method slots must be of location type and list type")})
    
    val slot1BestAnswers = answers(slot1)
    val slot2BestAnswers = answers(slot2)
    var doRemove = false
    
    var truncatedArray = scala.collection.mutable.ArrayBuffer[Candidate]()
    for(ans <- slot2BestAnswers){
      truncatedArray ++= List(ans)
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
            //remove from truncated Array
            truncatedArray -= slot2Answer
          }
        }
      }
    }
    
    truncatedArray.toSeq
  }
}