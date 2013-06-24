name := "KnowItAll_TAC2013_SlotFiller"

assemblySettings

net.virtualvoid.sbt.graph.Plugin.graphSettings

version := "0.0.1"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

resolvers += "Internal Maven" at "http://knowitall.cs.washington.edu/maven2"

scalaVersion := "2.10.2"

organization := "edu.knowitall"

resolvers += "amateras-repo" at "http://amateras.sourceforge.jp/mvn"

libraryDependencies ++= Seq(
        "edu.washington.cs.knowitall.nlptools" % "nlptools-sentence-breeze_2.10" % "2.4.2" excludeAll(ExclusionRule(organization = "com.googlecode.clearnlp")),
	"com.googlecode.clearnlp" % "clearnlp-threadsafe" % "1.3.0-a",
        "net.databinder" %% "unfiltered-filter" % "0.6.8",
        "net.databinder" %% "unfiltered-jetty" % "0.6.8",
	"jp.sf.amateras.solr.scala" %% "solr-scala-client" % "0.0.7",
	"edu.stanford.nlp" % "stanford-corenlp" % "1.3.4",
	"org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
	"edu.washington.cs.knowitall.nlptools" % "nlptools-chunk-opennlp_2.10" % "2.4.2",
	"edu.washington.cs.knowitall.nlptools" % "nlptools-parse-clear_2.10" % "2.4.2" excludeAll(ExclusionRule(organization = "com.googlecode.clearnlp")),
	"edu.washington.cs.knowitall.srlie" %% "openie-srl" % "1.0.0-RC1" excludeAll(ExclusionRule(organization = "com.googlecode.clearnlp")),
	"net.liftweb" %% "lift-json" % "2.5-RC5",
	"org.apache.solr" % "solr-solrj" % "4.3.0",
	"edu.washington.cs.knowitall.chunkedextractor" %% "chunkedextractor" % "1.0.4",
	"org.slf4j" % "slf4j-api" % "1.7.2",
	"ch.qos.logback" % "logback-classic" % "1.0.9",
        "ch.qos.logback" % "logback-core" % "1.0.9")

javaOptions in run += "-Xmx8G"

fork in run := true
