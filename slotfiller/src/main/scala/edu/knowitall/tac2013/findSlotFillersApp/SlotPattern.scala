package edu.knowitall.tac2013.findSlotFillersApp

import edu.knowitall.common.Resource
import io.Source
import KBPQueryEntityType._

// many of these variables can be empty like ""
case class SlotPattern private (
  val slotName: String,
  val maxValues: Option[Int],
  val openIERelationString: Option[String],
  val arg2Begins: Option[String],
  val entityIn: Option[String],
  val slotFillIn: Option[String],
  val slotType: Option[String]) {

  import SlotPattern.requireTrimmed

  requireTrimmed(slotName)
  openIERelationString foreach requireTrimmed
  arg2Begins foreach requireTrimmed
  entityIn foreach requireTrimmed
  slotFillIn foreach requireTrimmed
  slotType foreach requireTrimmed
  
  require(slotName.startsWith("per:") || slotName.startsWith("org:"))

  val entityType: KBPQueryEntityType = if (slotName.startsWith("per:")) PER else ORG
  
  def isValid(): Boolean = {
    if (openIERelationString.nonEmpty && maxValues.nonEmpty &&
      entityIn.nonEmpty && slotFillIn.nonEmpty) {
      true
    } else {
      false
    }
  }
  
  def debugString = "RelationTerms: " + openIERelationString.getOrElse({ "" }) +
        "\t Arg2Begins: " + arg2Begins.getOrElse({ "" }) + "\t Entity In: " +
        entityIn.getOrElse({ "" }) + "\t SlotFill In: " + slotFillIn.getOrElse({ "" }) +
        "\t Slot type: " + slotType.getOrElse({ "" }) + "\n"
}

object SlotPattern {

  private def requireTrimmed(s: String) = require(s.equals(s.trim()), "String must be trimmed: \"$s\"".format(s))

  private val personPatternResource = "/edu/knowitall/tac2013/findSlotFillersApp/KBP-OpenIE-Person.csv"
  private val organizationPatternResource = "/edu/knowitall/tac2013/findSlotFillersApp/KBP-OpenIE-Organization.csv"

  lazy val organizationPatterns = getPatternsAsMap(organizationPatternResource)

  lazy val personPatterns = getPatternsAsMap(personPatternResource)

  def patternsForQuery(query: KBPQuery): Map[String, List[SlotPattern]] = {
    val entityPatterns = query.entityType match {
      case KBPQueryEntityType.ORG => organizationPatterns
      case KBPQueryEntityType.PER => personPatterns
    }
    entityPatterns.filter {
      case (slotname, patterns) =>
        query.slotsToFill.contains(slotname)
    }
  }

  private def getPatternsAsMap(patternResource: String): Map[String, List[SlotPattern]] = {

    Resource.using(Source.fromURL(getClass.getResource(patternResource))) { source =>
      val patternLines = source.getLines.drop(1).filterNot(_.trim().startsWith(","))
      val patterns = patternLines flatMap SlotPattern.read
      val patternsMap = patterns.toSeq.groupBy(_.slotName)
      // turn Seq values into Lists
      patternsMap.map { case (key, value) => (key, value.toList) }
    }
  }

  private def read(str: String): Option[SlotPattern] = {

    val csvDataArray = str.replace(",", " ,").split(",").map(_.trim)
    csvDataArray match {
      case Array(slotName, maxValues, relString, arg2Begins, entityIn, slotFillIn, slotType, _*) => {
        val pattern = getSlotPattern(slotName, maxValues, relString, arg2Begins, entityIn, slotFillIn, slotType)
        Some(pattern)
      }
      case Array(slotName, maxValues, _*) => {
        val pattern = getSlotPattern(slotName, maxValues, "", "", "", "", "")
        Some(pattern)
      }
      case _ => {
        System.err.println("Couldn't parse pattern (%d fields): %s".format(csvDataArray.length, str))
        None
      }
    }
  }

  //instance instance of KbpSlotToOpenIEData instance with None type where the string
  // is empty
  // requires that input strings are trimmed.
  private def getSlotPattern(
    KbpSlotNameArgString: String,
    MaxValuesArgString: String,
    OpenIERelationArgString: String,
    Arg2BeginsArgString: String,
    EntityInArgString: String,
    SlotFillInArgString: String,
    SlotTypeArgString: String): SlotPattern = {

    requireTrimmed(KbpSlotNameArgString)
    requireTrimmed(MaxValuesArgString)
    requireTrimmed(OpenIERelationArgString)
    requireTrimmed(Arg2BeginsArgString)
    requireTrimmed(EntityInArgString)
    requireTrimmed(SlotFillInArgString)
    requireTrimmed(SlotTypeArgString)

    val arg1 = KbpSlotNameArgString

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

    new SlotPattern(arg1, arg2, arg3, arg4, arg5, arg6, arg7)
  }
}