package org.enso.languageserver.profiling

import akka.actor.{Actor, Props}
import org.enso.profiling.{MethodsSampler, OutputStreamSampler}

import java.io.ByteArrayOutputStream

/** Handles the profiling commands. */
final class ProfilingManager extends Actor {

  import ProfilingManager._

  override def receive: Receive =
    initialized(None)

  private def initialized(sampler: Option[RunningSampler]): Receive = {
    case ProfilingProtocol.ProfilingStartRequest =>
      sampler match {
        case Some(_) =>
          sender() ! ProfilingProtocol.ProfilingStartResponse
        case None =>
          val result  = new ByteArrayOutputStream()
          val sampler = new OutputStreamSampler(result)
          sampler.start()
          sender() ! ProfilingProtocol.ProfilingStartResponse
          context.become(initialized(Some(RunningSampler(sampler, result))))
      }

    case ProfilingProtocol.ProfilingStopRequest =>
      sampler match {
        case Some(RunningSampler(sampler, result)) =>
          sampler.stop()
          sender() ! ProfilingProtocol.ProfilingStopResponse(result.toByteArray)
          context.become(initialized(None))
        case None =>
          sender() ! ProfilingProtocol.ProfilingStopResponse(Array())
      }
  }
}

object ProfilingManager {

  private case class RunningSampler(
    sampler: MethodsSampler,
    result: ByteArrayOutputStream
  )

  /**
    *
    * @return
    */
  def props: Props =
    Props(new ProfilingManager)
}
