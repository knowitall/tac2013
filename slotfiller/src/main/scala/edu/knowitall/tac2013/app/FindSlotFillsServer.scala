package edu.knowitall.tac2013.app

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
import java.io.OutputStream
import java.io.PrintStream

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
          println(req.parameterNames.mkString(" "))
          handlePost(
            req.parameterValues("name").head,
            req.parameterValues("nodeId").head,
            req.parameterValues("type").head,
            req.parameterValues("slots").head,
            req.parameterValues("corpus").head,
            req.parameterValues("pu").headOption,
            req.parameterValues("pf").headOption,
            req.parameterValues("dc").headOption,
            req.parameterValues("pg").headOption,
            req.parameterValues("dg").headOption,
            req.parameterValues("pa").headOption,
            req.parameterValues("da").headOption,
            req.parameterValues("cf").headOption)
        case req @ GET(Path(Seg(Nil))) =>
          ResponseString("""<html>
              <h1>KnowItAll Slot Fill Test Server</h1>
              <body>
            <form method="POST">
              Entity Name: <input type="text" name="name"/><br/>
              Entity Node Id: (optional) <input type="text" name="nodeId"/><br/>
              Entity Type:<br/>
              <input type="radio" name="type" value="person" checked>person<br/>
        	  <input type="radio" name="type" value="organization">organization<br/>
              Slots: (optional) <input type="text" name="slots"/> (comma sep. default: all slots) <br/>
              Corpus:<br/>
              <input type="radio" name="corpus" value="new" checked>2013 Corpus<br/>
        	  <input type="radio" name="corpus" value="old">2010 Corpus<br/>
              Output Options:<br/>
              <input type="checkbox" name="pu" value="true">Print Unfiltered Candidates?<br/>
              <input type="checkbox" name="pf" value="true" checked>Print Filtered Candidates?<br/>
              <input type="checkbox" name="dc" value="true" checked>Detailed filtered/unfiltered output?<br/>
              <input type="checkbox" name="pg" value="true" checked>Print merged answer groups? <input type="checkbox" name="dg" value="true" checked>Detailed?<br/>
              <input type="checkbox" name="pa" value="true" checked>Print answers? <input type="checkbox" name="da" value="true" checked>Detailed?<br/>
              Query Options:<br/>
              <input type="checkbox" name="cf" value="true" checked>Coref<br/>
              <input type="submit"/>
            </form>
            </body></html>""") ~> Ok
      }

      /**
       * *
       * Handles the POST input to the server
       */
      def handlePost(
          entityString: String, 
          nodeIdStr: String, 
          typ: String, slots: 
          String, corpus: String,
          pu: Option[String], // print unfiltered?
          pf: Option[String], // print filtered?
          dc: Option[String], // detailed candidates?
          pg: Option[String], // print groups?
          dg: Option[String], // detailed groups?
          pa: Option[String], // print answers?
          da: Option[String],  // detailed answers?
          cf: Option[String]) = { //coref?

        val nodeId = if (nodeIdStr.nonEmpty) Some(nodeIdStr) else None
        
        val slotsSplit = slots.split(",").map(_.trim).filter(_.nonEmpty).toSet
        
        val printUnfiltered = pu.nonEmpty
        val printFiltered = pf.nonEmpty
        val detailedCandidates = dc.nonEmpty
        val printGroups = pg.nonEmpty
        val detailedGroups = dg.nonEmpty
        val printAnswers = pa.nonEmpty
        val detailedAnswers = da.nonEmpty
        val corefOn = cf.nonEmpty

        def getFormatter(out: PrintStream) = new OutputFormatter(out,
          printUnfiltered = printUnfiltered,
          printFiltered = printFiltered,
          detailedCandidates = detailedCandidates,
          printGroups = printGroups,
          detailedGroups = detailedGroups,
          printAnswers = printAnswers,
          detailedAnswers = detailedAnswers)
        
        
        new ResponseStreamer {
          def stream(os: OutputStream) = {
            val printStream = new PrintStream(os)
            try {
              new FindSlotFills(corpus,corefOn).runForServerOutput(entityString, nodeId, typ, slotsSplit, getFormatter(printStream))
            } catch {
              case e: Throwable => {
                e.printStackTrace(printStream)
                e.printStackTrace
                throw e
              }
            }
          }
        }
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
