package org.enso.languageserver.websocket.binary

import com.google.flatbuffers.FlatBufferBuilder
import org.apache.commons.io.FileUtils
import org.enso.languageserver.protocol.binary.{
  FileContentsReply,
  InboundPayload,
  OutboundMessage,
  OutboundPayload
}
import org.enso.languageserver.websocket.binary.factory._
import org.enso.testkit.FlakySpec

import java.io.File
import java.nio.ByteBuffer
import java.util.UUID
import scala.io.Source

class BinaryFileManipulationTest extends BaseBinaryServerTest with FlakySpec {

  implicit private val decoder = OutboundMessageDecoder

  "A WriteFileCommand" must {

    "persist binary contents of a file" taggedAs Flaky in {
      //given
      val requestId = UUID.randomUUID()
      val filename  = "foo.bin"
      val fooFile   = new File(testContentRoot.toFile, filename)
      val contents  = Array[Byte](65, 66, 67) //ABC
      val client    = newWsClient()
      client.send(createSessionInitCmd())
      client.expectFrame()
      val writeFileCommand = createWriteFileCmdPacket(
        requestId,
        filename,
        testContentRootId,
        contents
      )
      //send
      client.send(writeFileCommand)
      val Right(msg) = client.receiveMessage[OutboundMessage]()
      //then
      msg.payloadType() shouldBe OutboundPayload.SUCCESS
      msg
        .correlationId()
        .leastSigBits() shouldBe requestId.getLeastSignificantBits
      msg
        .correlationId()
        .mostSigBits() shouldBe requestId.getMostSignificantBits
      Source.fromFile(fooFile).mkString shouldBe "ABC"
    }
  }

  "A ReadFileCommand" must {

    "read binary contents of a file" in {
      //given
      val requestId = UUID.randomUUID()
      val filename  = "bar.bin"
      val barFile   = new File(testContentRoot.toFile, filename)
      val contents  = Array[Byte](65, 66, 67) //ABC
      FileUtils.writeByteArrayToFile(barFile, contents)
      val client = newWsClient()
      client.send(createSessionInitCmd())
      client.expectFrame()
      val readFileCommand =
        createReadFileCmdPacket(requestId, filename, testContentRootId)
      //when
      client.send(readFileCommand)
      val Right(msg) = client.receiveMessage[OutboundMessage]()
      //then
      msg.payloadType() shouldBe OutboundPayload.FILE_CONTENTS_REPLY
      msg
        .correlationId()
        .leastSigBits() shouldBe requestId.getLeastSignificantBits
      msg
        .correlationId()
        .mostSigBits() shouldBe requestId.getMostSignificantBits

      val reply =
        msg.payload(new FileContentsReply).asInstanceOf[FileContentsReply]
      reply
        .contentsAsByteBuffer()
        .compareTo(ByteBuffer.wrap(contents)) shouldBe 0
    }

  }

  "A ChecksumBytesCommand" must {
    "Return the checksum for the provided byte range" in {
      pending
      val requestId = UUID.randomUUID()
      val filename  = "bar.bin"
      val barFile   = new File(testContentRoot.toFile, filename)
      val contents  = Array[Byte](65, 66, 67) //ABC
      FileUtils.writeByteArrayToFile(barFile, contents)
      val client = newWsClient()
      client.send(createSessionInitCmd())
      client.expectFrame()
      val checksumBytesCmd = createChecksumBytesCommandPacket(
        requestId,
        Seq(filename),
        testContentRootId,
        byteOffset = 0,
        length = 2
      )

      client.send(checksumBytesCmd)
      val checksumResponse = client.receiveMessage[OutboundMessage]()
      checksumResponse shouldBe Right
    }

    "Return a `FileNotFound` error if the file does not exist" in {
      pending
    }

    "Return a `ReadOutOfBounds` error if the byte range is out of bounds" in {
      pending
    }

    "Return a `NotFile` error if the provided path is not a file" in {
      pending
    }
  }

  def createChecksumBytesCommandPacket(
    requestId: UUID,
    pathSegments: Seq[String],
    rootId: UUID,
    byteOffset: Long,
    length: Long
  ): ByteBuffer = {
    implicit val builder: FlatBufferBuilder = new FlatBufferBuilder(1024)

    val path        = PathFactory.create(rootId, pathSegments)
    val fileSegment = FileSegmentFactory.create(path, byteOffset, length)
    val command     = ChecksumBytesCommandFactory.create(fileSegment)

    val incomingMessage = InboundMessageFactory.create(
      requestId,
      None,
      InboundPayload.CHECKSUM_BYTES_CMD,
      command
    )

    builder.finish(incomingMessage)
    builder.dataBuffer()
  }

  def createWriteFileCmdPacket(
    requestId: UUID,
    pathSegment: String,
    rootId: UUID,
    contents: Array[Byte]
  ): ByteBuffer = {
    implicit val builder: FlatBufferBuilder = new FlatBufferBuilder(1024)

    val path = PathFactory.create(rootId, Seq(pathSegment))
    val cmd  = WriteFileCommandFactory.create(path, contents)

    val inMsg = InboundMessageFactory.create(
      requestId,
      None,
      InboundPayload.WRITE_FILE_CMD,
      cmd
    )

    builder.finish(inMsg)
    builder.dataBuffer()
  }

  def createReadFileCmdPacket(
    requestId: UUID,
    pathSegment: String,
    rootId: UUID
  ): ByteBuffer = {
    implicit val builder: FlatBufferBuilder = new FlatBufferBuilder(1024)

    val path = PathFactory.create(rootId, Seq(pathSegment))
    val cmd  = ReadFileCommandFactory.create(path)

    val inMsg = InboundMessageFactory.create(
      requestId,
      None,
      InboundPayload.READ_FILE_CMD,
      cmd
    )

    builder.finish(inMsg)
    builder.dataBuffer()
  }

}
