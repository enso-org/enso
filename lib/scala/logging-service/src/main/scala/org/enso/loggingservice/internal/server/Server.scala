package org.enso.loggingservice.internal.server

import java.io.ObjectInputFilter.Config

import akka.actor.ActorSystem
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.Source
import org.enso.loggingservice.internal.serviceconnection.Server

class Server {}

object Server {
//  implicit lazy val as = ActorSystem("logger-server")
//  def setup(hostname: String, port: Short, config: Config): Server = {
//    // TODO
//
//    val x               = Source.queue[Int](3, OverflowStrategy.dropNew)
//    val (queue, source) = x.preMaterialize()
//    queue.offer(123)
//    source.groupedWithin()
//    source.runForeach(println(_)).onComplete(println(_))
//  }
}
