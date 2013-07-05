package edu.knowitall.tac2013.findSlotFillersApp

import java.io.File
import scala.Array.canBuildFrom
import scala.io.Source
import scopt.immutable.OptionParser
import unfiltered.filter.Intent
import unfiltered.jetty.Http
import unfiltered.request.{ GET, POST }
import unfiltered.request.Path
import unfiltered.request.Seg
import unfiltered.response.NotFound
import unfiltered.response.Ok
import unfiltered.response.ResponseString
import org.slf4j.LoggerFactory
import unfiltered.filter.Planify
import unfiltered.response.ResponseStreamer
import unfiltered.response.ResponseWriter

object FindSlotFillsServer extends App {
  val logger = LoggerFactory.getLogger(this.getClass)

  case class Config(port: Int = 8080)

  val argumentParser = new OptionParser[Config]("miniserver") {
    def options = Seq(
      intOpt("p", "port", "output file (otherwise stdout)") { (port: Int, config: Config) =>
        config.copy(port = port)
      })
  }

  argumentParser.parse(args, Config()) match {
    case Some(config) => run(config)
    case None =>
  }

  def run(configuration: Config) {
    object Plan extends unfiltered.filter.Plan {
      def intent = Intent {
        case req @ POST(Path(Seg(Nil))) =>
          println(req.parameterNames)
          handlePost(req.parameterValues("field1").head,
            req.parameterValues("field2").head)
        case req @ GET(Path(Seg(Nil))) =>
          ResponseString("""<html><body>
            <form method="POST">
              <textarea cols="60" rows="20" name="field1"></textarea><br />
              <input type="text" name="field2"/>
              <input type="submit"/>
            </form>
            </body></html>""") ~> Ok
      }

      /**
       * *
       * Handles the POST input to the server
       */
      def handlePost(field1: String, field2: String) = {

        val field1Split = field1.split(" ")
        var entityString = field1
        var nodeId = ""
        if (field1Split.length > 1) {
          var isNodeId = false
          for (c <- field1Split(field1Split.length - 1)) {
            if (c.isValidInt) {
              isNodeId = true
            }
          }
          if (isNodeId) {
            nodeId = field1Split(field1Split.length - 1)
            entityString = field1.substring(0, field1.size - nodeId.size)
          }
        }
        val slotOutputs = FindSlotFills.runForServerOutput(entityString, field2, if (nodeId.nonEmpty) Some(nodeId) else None)
        val allOutput = slotOutputs.mkString
        
        ResponseString(field1 + allOutput) ~> Ok
      }
    }

    println("starting...")
    try {
      Http(configuration.port).filter(Plan).run()
    } catch {
      case e: java.net.BindException => println("Address already in use: " + configuration.port); System.exit(1)
    }

  }
}
