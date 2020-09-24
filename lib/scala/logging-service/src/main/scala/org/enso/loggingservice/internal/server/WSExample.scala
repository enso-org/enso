package org.enso.loggingservice.internal.server

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.ws.{
  BinaryMessage,
  Message,
  TextMessage,
  UpgradeToWebSocket
}
import akka.http.scaladsl.model.{HttpRequest, HttpResponse, Uri}
import akka.stream.{Attributes, OverflowStrategy}
import akka.stream.scaladsl.{Flow, Sink, Source}
import io.circe.Error
import org.enso.loggingservice.internal.WSLogMessage

import scala.annotation.nowarn
import scala.io.StdIn
import io.circe.parser

object WSExample {
  private def decodeMessage(message: String): Either[Error, WSLogMessage] =
    parser.parse(message).flatMap(_.as[WSLogMessage])

  def test(): Unit = {
    implicit val system = ActorSystem("testsys")
    val x               = Source.queue[Int](10, OverflowStrategy.fail)
    val (queue, source) = x.preMaterialize()
    queue.offer(1)
    queue.offer(2)
    queue.offer(3)
    queue.offer(4)
    queue.offer(5)
    val proc = source.runForeach(println(_))
    queue.offer(6)
    Thread.sleep(100)
    queue.offer(7)
    queue.complete()
    import system.dispatcher
    proc.onComplete(_ => {
      system.terminate()
      println("Finished")
    })
  }

  def startServer(): Unit = {

    implicit val system = ActorSystem("graal")

    val messageProcessor = Sink.foreach[Message] {
      case tm: TextMessage =>
        System.err.println(s"Got message $tm")
        val rawMessage     = tm.textStream.fold("")(_ + _)
        val decodedMessage = rawMessage.map(decodeMessage)
        decodedMessage.runForeach {
          case Left(error) =>
            // TODO [RW] warn on unepxected message format, preferrably only once
            print(error)
          case Right(message) =>
          // TODO
        }
      case bm: BinaryMessage =>
        // TODO [RW] maybe warn on unexpected binary message
        bm.dataStream.runWith(Sink.ignore)
    }

    val greeterWebSocketService =
      Flow.fromSinkAndSourceCoupled(messageProcessor, Source.never)
//      Flow[Message]
//        .mapConcat {
//          case tm: TextMessage =>
//            System.err.println(s"Got message $tm")
//            TextMessage(Source.single("Hello ") ++ tm.textStream) :: Nil
//          case bm: BinaryMessage =>
//            // TODO [RW] maybe warn on unexpected binary message
//            bm.dataStream.runWith(Sink.ignore)
//            Nil
//        }

    val requestHandler: HttpRequest => HttpResponse = {
      case req @ HttpRequest(GET, Uri.Path("/"), _, _, _) =>
        req.header[UpgradeToWebSocket @nowarn] match {
          case Some(upgrade) => upgrade.handleMessages(greeterWebSocketService)
          case None =>
            HttpResponse(400, entity = "Not a valid websocket request!")
        }
      case r: HttpRequest =>
        r.discardEntityBytes() // important to drain incoming HTTP Entity stream
        HttpResponse(404, entity = "Unknown resource!")
    }

    val bindingFuture =
      Http().bindAndHandleSync(
        requestHandler,
        interface = "localhost",
        port      = 8080
      )

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine()

    import system.dispatcher // for the future transformations
    bindingFuture
      .flatMap(_.unbind())                 // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}
