package edu.knowitall.tac2013.experiment

import jp.sf.amateras.solr.scala.SolrClient
import java.io.File

object DocumentSizeData {

  def main(args: Array[String]){


    val solrUrl = "http://knowitall:knowit!@rv-n16.cs.washington.edu:9325/solr/newCorpus"
    val client = new SolrClient(solrUrl)
    val allQuery = client.query("*:*").fields("docid")
    val result = allQuery.rows(3000000).getResultAsMap()

    println("here")

    val pw = new java.io.PrintWriter(new File("docSizes"))


    for(r <- result.documents){
        val docSizeQuery = client.query("docid:"+"\"" + r("docid") +"\"")
        val result = docSizeQuery.rows(1).getResultAsMap()
        val docResult = result.documents.head
        pw.write(docResult("xml").toString().size + "\n")
    }

    pw.close()



  }

}
