package controllers

import models.Query
import play.api.Logger
import models.ExtractionInstance
import play.api.Play.current
import play.api.Play
import models.TypeHierarchy
import java.util.concurrent.atomic.AtomicInteger

object LuceneQueryExecutor {

  val defaultSolrUrl = current.configuration.getString("db.solr.url").get
  
  def solrUrl(q: Query): String = solrUrl(q.corpus)

  def solrUrl(corpus: Option[String]): String = {
    val url = corpus match {
      case Some("2010") => current.configuration.getString("db.solr.oldurl").get
      case _ => current.configuration.getString("db.solr.url").get
    }
    Logger.info("SOLR Url: " + url)
    url
  }
  
  def fullLuceneQueryString(query: Query): String = {
    LuceneQueryExecutor.luceneQueryVariables(query).foldLeft(
      LuceneQueryExecutor.luceneQueryString(query)) {
        case (query, (field, value)) =>
          query.replaceAll("%" + field + "%", "\"" + value + "\"")
      }
  }

  def luceneQueryString(q: Query): String = {
    val strings =
      q.usedStrings.map { p => p.string.zipWithIndex.map
        { case (string, i) => "+" + p.part.short + ":%" + p.part.short + "_" + i + "%" }.mkString(" ")
      }

    (strings).mkString(" ") + " " + q.linkDocQuery
  }

  def luceneQueryVariables(q: Query): Map[String, String] =
    Map.empty ++
       q.usedStrings.flatMap(part => part.string.zipWithIndex.map { case (string, i) => (part.part.short + "_" + i) -> part.part(q).string(i) }) ++
       q.usedTypes.flatMap(part => part.typ.flatMap(Application.typeHierarchy.baseTypes).zipWithIndex.map { case (typ, i) => (part.part.short + "_types_" + i) -> typ })
       
  def filterExtractor(eOpt: Option[String])(inst: ExtractionInstance) = eOpt match {
    case Some(extractor) => inst.extractor == extractor
    case None => true
  }

  def execute(q: Query) = {
    Logger.info("query for: " + q)

    import jp.sf.amateras.solr.scala._

    val client = new SolrClient(solrUrl(q))

    val queryString = luceneQueryString(q)
    val queryVariables = luceneQueryVariables(q)

    Logger.logger.debug("Lucene query: " + queryString)
    Logger.logger.debug("Lucene variables: " + queryVariables)

    val result = client.query(queryString)
      .rows(10000)
      .getResultAsMap(queryVariables)
      
    val list = result.documents.toList
    Logger.info("results received: " + list.size)
    
    list.map(ExtractionInstance.fromMap) filter filterExtractor(q.extractor)
  }

  def executeExact(arg1: String, rel: String, arg2: String, arg1link: Option[String], arg2link: Option[String], docId: Option[String], corpus: Option[String], extractor: Option[String]) = {

    Logger.info("sentenes for: " + List(arg1, rel, arg2))
    import jp.sf.amateras.solr.scala._

    val client = new SolrClient(solrUrl(corpus))

    val optQueryString = (arg1link.map(l=>"+arg1WikiLinkNodeId:\""+l+"\"") ++ arg2link.map(l=>"+arg2WikiLinkNodeId:\""+l+"\"") ++ docId.map(l=>"+docId:\""+l+"\"")).mkString(" ")
    val queryString = "+arg1Text:\"%s\" +relText:\"%s\" +arg2Text:\"%s\" ".format(arg1, rel, arg2) + " " + optQueryString
    
    Logger.logger.debug("Lucene query: " + queryString)

    val result = client.query(queryString)
      .rows(10000)
      .getResultAsMap(Map.empty)

    val list = result.documents.toList
    Logger.info("sentence extractions received: " + list.size)
    list.map(ExtractionInstance.fromMap) filter filterExtractor(extractor)
  }

  def execute(q: String) = {
    Logger.info("query for: " + q)

    import jp.sf.amateras.solr.scala._

    val client = new SolrClient(defaultSolrUrl)

    Logger.logger.debug("Lucene query: " + q)

    val result = client.query(q)
      .rows(10000)
      .getResultAsMap()

    val list = result.documents.toList
    Logger.info("results received: " + list.size)
    list.map(ExtractionInstance.fromMap)
  }
}
