package edu.knowitall.tac2013.solr.query

import edu.knowitall.tac2013.solr.query.SolrQueryType._
import edu.knowitall.tac2013.app.KBPQuery
import edu.knowitall.tac2013.app.SlotPattern
import scala.Option.option2Iterable
import scala.util.matching.Regex

case class SolrQuery(val queryString: String, val queryType: SolrQueryType, val pattern: SlotPattern)

class SolrQueryBuilder(val pattern: SlotPattern, val kbpQuery: KBPQuery, val corefOn: Boolean) {

  
  def this(pattern: SlotPattern, kbpQuery: KBPQuery) = this(pattern, kbpQuery ,false)
  val arg1TextConstraint: Option[String] = {
    pattern.entityIn match {
      case Some("arg1") => Some("+arg1Text:\"%s\"".format(kbpQuery.name))
      case _ => None
    }
  }

  val relTextConstraint: Option[String] = {
    pattern.relString match {
      case Some(relString) => {
        val noSemanticCategoriesString =  SolrQueryBuilder.semanticCategoryPattern.replaceAllIn(relString, "").trim
        if (noSemanticCategoriesString.trim() != "") {
          Some("+relText:\"" + noSemanticCategoriesString + "\"")
        } else {
          None
        }
      }
      case None => None
    }
  }

  val arg2TextConstraint: Option[String] = {
    pattern.entityIn match {
      case Some("arg2") => Some("+arg2Text:\"%s\"".format(kbpQuery.name))
      case _ => None
    }
  }

  val arg2StartConstraint: Option[String] = {
    pattern.arg2Begins match {
      case Some(arg2Begins) => {
        val noSemanticCategoriesString =  SolrQueryBuilder.semanticCategoryPattern.replaceAllIn(arg2Begins, "").trim
        if (noSemanticCategoriesString.trim() != "") {
          Some("+arg2Text:\"" + noSemanticCategoriesString + "\"")
        } else {
          None
        }
      }
      case None => None
    }
  }
  
  val arg1TermsConstraint: Option[String] = {
    pattern.arg1Terms match {
      case Some(arg1Terms) => {
        val noSemanticCategoriesString =  SolrQueryBuilder.semanticCategoryPattern.replaceAllIn(arg1Terms, "").trim
        if (noSemanticCategoriesString.trim() != "") {
          Some("+arg1Text:\"" + noSemanticCategoriesString + "\"")
        } else {
          None
        }
      }
      case None => None
    }
  }
  
  val arg2TermsConstraint: Option[String] = {
    pattern.arg2Terms match {
      case Some(arg2Terms) => {
        val noSemanticCategoriesString =  SolrQueryBuilder.semanticCategoryPattern.replaceAllIn(arg2Terms, "").trim
        if (noSemanticCategoriesString.trim() != "") {
          Some("+arg2Text:\"" + noSemanticCategoriesString + "\"")
        } else {
          None
        }
      }
      case None => None
    }
  }

  val arg1LinkConstraint: Option[String] = {
    (pattern.entityIn, kbpQuery.nodeId) match {
      case (Some("arg1"), Some(id)) => Some("+arg1WikiLinkNodeId:\"%s\"".format(id))
      case _ => None
    }
  }

  val arg2LinkConstraint: Option[String] = {
    (pattern.entityIn, kbpQuery.nodeId) match {
      case (Some("arg2"), Some(id)) => Some("+arg2WikiLinkNodeId:\"%s\"".format(id))
      case _ => None
    }
  }
  
  
  def getDocIdConstraint(docId: String): Option[String] = {
    Some("+docId:\"%s\"".format(docId))
  }
  
  private def getQueryString(fields: Seq[String]) = {
    
    val nonEmptyFields = fields.filter(_.nonEmpty)
    nonEmptyFields.mkString(" AND ")
  }

  val regularQuery: Option[SolrQuery] = {

    if (!pattern.isValid) {
      None
    } else {
      val queryFields = Seq(arg1TextConstraint, relTextConstraint, arg2TextConstraint, arg2StartConstraint,arg1TermsConstraint,arg2TermsConstraint).flatten
      val query = SolrQuery(getQueryString(queryFields), SolrQueryType.REGULAR, pattern)
      Some(query)
    }
  }

  val linkedQuery: Option[SolrQuery] = {

    if (!pattern.isValid || kbpQuery.nodeId.isEmpty) {
      None
    } else {
      val queryFields = Seq(arg1LinkConstraint, relTextConstraint, arg2LinkConstraint, arg2StartConstraint,arg1TermsConstraint,arg2TermsConstraint).flatten
      val query = SolrQuery(getQueryString(queryFields), SolrQueryType.LINKED, pattern)
      Some(query)
    }
  }
  
  val corefQueries: Option[Seq[SolrQuery]] = {
    
    if(corefOn){
	    if(!pattern.isValid || kbpQuery.docIds.isEmpty){
	      None
	    }
	    else{
	      val seqOfCorefQueries = 
	      for(docId <- kbpQuery.docIds) yield {
	    	val queryFields = Seq(getDocIdConstraint(docId),relTextConstraint,arg2StartConstraint,arg1TermsConstraint,arg2TermsConstraint).flatten
	    	val query = SolrQuery(getQueryString(queryFields), SolrQueryType.COREF, pattern)
	    	query
	      }
	      Some(seqOfCorefQueries)
	    }
    }
    else{
      None
    }
  }
  
  val getQueries: Seq[SolrQuery] = {

    if(corefQueries.isDefined){
      regularQuery.toSeq ++ linkedQuery ++ corefQueries.get
    }
    else{
       regularQuery.toSeq ++ linkedQuery
    }
  }
}

object SolrQueryBuilder{
  
  lazy val semanticCategoryPattern = new Regex("[A-Z<]\\w+(>)?")
}