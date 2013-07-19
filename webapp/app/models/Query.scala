package models

import scala.util.matching.Regex

case class AdvancedQuery(queryString: String, groupBy: ExtractionPart)
case class Query(
    arg1: PartQuery,
    rel: PartQuery,
    arg2: PartQuery,
    arg1link: Option[String], 
    arg2link: Option[String], 
    docId: Option[String],
    extractor: Option[String],
    corpus: Option[String],
    groupBy: ExtractionPart) {
  def used: Seq[PartQuery] = {
    var seq = Seq.empty[PartQuery]

    if (arg1.used) seq :+= arg1
    if (rel.used) seq :+= rel
    if (arg2.used) seq :+= arg2

    seq
  }

  def usedStrings = used.filter(!_.string.isEmpty)
  def usedTypes = used.filter(!_.typ.isEmpty)
  
  
  def linkQuery(field: String)(link: String) = field + "WikiLinkNodeId:\"" + link + "\""
  def linkDocQuery = (arg1link.map(linkQuery("+arg1")) ++ arg2link.map(linkQuery("+arg2")) ++ docId.map(d => "+docId:\""+d+"\"")).mkString(" ")
}

case class PartQuery(string: Seq[String], typ: Seq[String], part: ExtractionPart) {
  def entryString =
    if (string.isEmpty && typ.isEmpty) None
    else Some((string ++ typ.map("[" + _ + "]")).mkString("; "))

  def used = !string.isEmpty || !typ.isEmpty
}
object PartQuery {
  val typeRegex = new Regex("""\s*\[(.*)\]\s*""")
  val empty = PartQuery(Seq.empty, Seq.empty, ExtractionPart.default)
  def fromEntry(entry: String, part: ExtractionPart) = {
    val (strings, types) =
      entry.split("""\s*;\s*""").foldLeft(IndexedSeq.empty[String], IndexedSeq.empty[String]) {
        case ((strings, types), entry) => entry match {
          case typeRegex(typ) => (strings, types :+ typ)
          case string => (strings :+ string, types)
        }
    }

    PartQuery(strings.toSeq, types.toSeq, part)
  }
}

object Query {
  def empty = Query(None, None, None, None, None, None, None, None)

  def apply(
      arg1: Option[String],
      rel: Option[String],
      arg2: Option[String],
      arg1link: Option[String],
      arg2link: Option[String],
      docId: Option[String],
      extractor: Option[String],
      corpus: Option[String],
      groupBy: ExtractionPart = Argument1) = {

    val arg1Part = arg1.map(PartQuery.fromEntry(_, Argument1)).getOrElse(PartQuery.empty)
    val relPart = rel.map(PartQuery.fromEntry(_, Relation)).getOrElse(PartQuery.empty)
    val arg2Part = arg2.map(PartQuery.fromEntry(_, Argument2)).getOrElse(PartQuery.empty)

    new Query(arg1Part, relPart, arg2Part, arg1link, arg2link, docId,
        extractor, corpus, groupBy)
  }

  val extractors = List("relnoun" -> "relnoun", "srl" -> "srl")
  val corpora = List("2013", "2010")
}
