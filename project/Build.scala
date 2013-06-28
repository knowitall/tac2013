import sbt._
import Keys._

import sbtassembly.Plugin._
import AssemblyKeys._

object SlotFillerBuild extends Build {
  // settings
  val buildOrganization = "edu.knowitall"
  val buildVersion = "1.0.0"
  val buildScalaVersions = Seq("2.10.2")

  val mavenLocal = "Local Maven Repository" at "file://"+Path.userHome+"/.m2/repository"

  lazy val tac2013 = Project(id = "tac2013", base = file(".")) settings (
    crossScalaVersions := buildScalaVersions,
    scalaVersion <<= (crossScalaVersions) { versions => versions.head },
    publish := { },
    publishLocal := { }
  ) aggregate(slotfiller, multir)

  // parent build definition
  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := "2.10.2",
    resolvers ++= Seq(
      "knowitall" at "http://knowitall.cs.washington.edu/maven2",
      "knowitall-snapshot" at "http://knowitall.cs.washington.edu/maven2-snapshot",
      mavenLocal)
  ) ++ assemblySettings

  lazy val slotfiller = Project(id = "slotfiller", base = file("slotfiller"), settings = buildSettings ++ Seq(
    libraryDependencies ++= Seq(
    "edu.washington.cs.knowitall.taggers" %% "taggers" % "0.1",
    "com.nicta" %% "scoobi" % "0.7.0-RC2-cdh3",
    "edu.washington.cs.knowitall.openie" %% "openie-linker" % "1.0",
    "edu.washington.cs.knowitall.nlptools" % "nlptools-sentence-breeze_2.10" % "2.4.2" excludeAll(ExclusionRule(organization = "com.googlecode.clearnlp")),
    "com.googlecode.clearnlp" % "clearnlp-threadsafe" % "1.3.0-c",
    "net.databinder" %% "unfiltered-filter" % "0.6.8",
    "net.databinder" %% "unfiltered-jetty" % "0.6.8",
    "jp.sf.amateras.solr.scala" %% "solr-scala-client" % "0.0.7",
    "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
    "edu.washington.cs.knowitall.nlptools" % "nlptools-chunk-opennlp_2.10" % "2.4.2",
    "edu.washington.cs.knowitall.nlptools" % "nlptools-parse-clear_2.10" % "2.4.2" excludeAll(ExclusionRule(organization = "com.googlecode.clearnlp")),
    "edu.washington.cs.knowitall.srlie" %% "openie-srl" % "1.0.0-RC1"   excludeAll(ExclusionRule(organization = "com.googlecode.clearnlp")),
    "net.liftweb" %% "lift-json" % "2.5-RC5",
    "org.apache.solr" % "solr-solrj" % "4.3.0",
    "edu.washington.cs.knowitall.chunkedextractor" %% "chunkedextractor" % "1.0.4",
    "org.slf4j" % "slf4j-api" % "1.7.2",
    "ch.qos.logback" % "logback-classic" % "1.0.9",
    "ch.qos.logback" % "logback-core" % "1.0.9"
    ),
    resolvers ++= Seq("nicta" at "http://nicta.github.com/scoobi/releases",
      "cloudera" at "https://repository.cloudera.com/content/repositories/releases",
      "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
      "amateras-repo" at "http://amateras.sourceforge.jp/mvn/"),
    mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
      {
        case x => {
          val oldstrat = old(x)
          if (oldstrat == MergeStrategy.deduplicate) MergeStrategy.first
          else oldstrat
        }
      }
    }
  )).settings(net.virtualvoid.sbt.graph.Plugin.graphSettings: _*)

  lazy val multir = Project(id = "multir", base = file("multir"), settings = buildSettings ++ Seq(
    libraryDependencies ++= Seq("edu.stanford.nlp" % "stanford-corenlp" % "1.3.4")
  )).settings(net.virtualvoid.sbt.graph.Plugin.graphSettings: _*)

}

