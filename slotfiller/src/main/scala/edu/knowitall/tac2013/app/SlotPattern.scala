package edu.knowitall.tac2013.app

import edu.knowitall.common.Resource
import io.Source
import KBPQueryEntityType._

// many of these variables can be empty like ""
case class SlotPattern private (
  val slotName: String,
  private val maxValues: Option[Int], // use slot.maxValues
  val relString: Option[String],
  val arg2Begins: Option[String],
  val entityIn: Option[String],
  val slotFillIn: Option[String],
  val slotType: Option[String],
  val arg1Terms: Option[String],
  val arg2Terms: Option[String]){

  import SlotPattern.requireTrimmed

  relString foreach requireTrimmed
  arg2Begins foreach requireTrimmed
  entityIn foreach requireTrimmed
  slotFillIn foreach requireTrimmed
  slotType foreach requireTrimmed
  arg1Terms foreach requireTrimmed
  arg2Terms foreach requireTrimmed

  val entityType: KBPQueryEntityType = if (slotName.startsWith("per:")) PER else ORG
  
  def isValid(): Boolean = {
    if (relString.nonEmpty && maxValues.nonEmpty &&
      entityIn.nonEmpty && slotFillIn.nonEmpty) {
      true
    } else {
      false
    }
  }
  
  def debugString = {
    val arg2 = (entityIn, slotFillIn) match {
      case (Some("arg2"), _) => "<entity>"
      case (_, Some("arg2")) => "<fill>"
      case (_, _) => "*"
    }
    val arg1 = (entityIn, slotFillIn) match {
      case (Some("arg1"), _) => "<entity>"
      case (_, Some("arg1")) => "<fill>"
      case (_, _) => "*"
    }
    val rel = (entityIn, slotFillIn) match {
      case (Some("relation"), _) => relString.getOrElse("") + " <entity>"
      case (_, Some("relation")) => relString.getOrElse("") + " <fill>"
      case (_, _) => relString.getOrElse("*")
    }
    slotType.getOrElse("UKN") ++ Seq(arg1, rel, arg2Begins.map(_ + " ").getOrElse("") + arg2).mkString("(", ", ", ")")
  }
}

object SlotPattern {

  private def requireTrimmed(s: String) = require(s.equals(s.trim()), "String must be trimmed: \"%s\"".format(s))

  val personPatternResource = "/edu/knowitall/tac2013/findSlotFillersApp/KBP-OpenIE-Person.csv"
  val organizationPatternResource = "/edu/knowitall/tac2013/findSlotFillersApp/KBP-OpenIE-Organization.csv"
    
  def read(csvDataArray: Array[String]): Option[SlotPattern] = {
    
    csvDataArray match {
      case Array(slotName, maxValues, relString, arg2Begins, entityIn, slotFillIn, slotType, arg1Terms, arg2Terms, _*) => {
        val pattern = getSlotPattern(slotName, maxValues, relString, arg2Begins, entityIn, slotFillIn, slotType, arg1Terms, arg2Terms)
        Some(pattern)
      }
      case Array(slotName, maxValues, relString, arg2Begins, entityIn, slotFillIn, slotType, _*) => {
        val pattern = getSlotPattern(slotName, maxValues, relString, arg2Begins, entityIn, slotFillIn, slotType, "", "")
        Some(pattern)
      }
      case Array(slotName, maxValues, _*) => {
        val pattern = getSlotPattern(slotName, maxValues, "", "", "", "", "", "", "")
        Some(pattern)
      }
      case _ => {
        System.err.println("Couldn't parse pattern fields(%d): %s".format(csvDataArray.length, csvDataArray.mkString(", ")))
        None
      }
    }
  }

  //instance instance of KbpSlotToOpenIEData instance with None type where the string
  // is empty
  // requires that input strings are trimmed.
  private def getSlotPattern(
    slot: String,
    MaxValuesArgString: String,
    OpenIERelationArgString: String,
    Arg2BeginsArgString: String,
    EntityInArgString: String,
    SlotFillInArgString: String,
    SlotTypeArgString: String,
    Arg1TermsArgString: String,
    Arg2TermsArgString: String): SlotPattern = {
    
    requireTrimmed(MaxValuesArgString)
    requireTrimmed(OpenIERelationArgString)
    requireTrimmed(Arg2BeginsArgString)
    requireTrimmed(EntityInArgString)
    requireTrimmed(SlotFillInArgString)
    requireTrimmed(SlotTypeArgString)
    requireTrimmed(Arg1TermsArgString)
    requireTrimmed(Arg2TermsArgString)

    //determine if this is a valid integer
    val arg2 = {
      try {
        Some(MaxValuesArgString.toInt)
      } catch {
        case e: NumberFormatException => { None }
      }
    }

    val arg3 = OpenIERelationArgString match {
      case "" => None
      case _ => Some(OpenIERelationArgString)
    }

    val arg4 = Arg2BeginsArgString match {
      case "" => None
      case _ => Some(Arg2BeginsArgString)
    }

    val arg5 = EntityInArgString match {
      case "" => None
      case _ => Some(EntityInArgString)
    }

    val arg6 = SlotFillInArgString match {
      case "" => None
      case _ => Some(SlotFillInArgString)
    }

    val arg7 = SlotTypeArgString match {
      case "" => None
      case _ => Some(SlotTypeArgString)
    }
    
    val arg8 = Arg1TermsArgString match{
      case "" => None
      case _ => Some(Arg1TermsArgString)
    }
    
    val arg9 = Arg2TermsArgString match{
      case "" => None
      case _ => Some(Arg2TermsArgString)
    }
    

    new SlotPattern(slot, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
  }
}