package org.enso.languageserver.websocket.binary.factory

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.binary.FileSegment

object FileSegmentFactory {

  /** Create a new binary representation of a file segment.
    *
    * @param path the path to the file in which the segment exists
    * @param byteOffset the start byte in the file (inclusive)
    * @param segmentLength the number of bytes to read from `byteOffset`
    * @param builder the flat buffers builder
    * @return a new binary representation of a file segment
    */
  def create(path: Int, byteOffset: Long, segmentLength: Long)(implicit
    builder: FlatBufferBuilder
  ): Int = {
    FileSegment.createFileSegment(builder, path, byteOffset, segmentLength)
  }
}
