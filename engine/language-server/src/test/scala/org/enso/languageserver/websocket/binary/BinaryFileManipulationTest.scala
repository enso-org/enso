package org.enso.languageserver.websocket.binary

import java.io.File
import java.nio.ByteBuffer
import java.util.UUID

import com.google.flatbuffers.FlatBufferBuilder
import org.apache.commons.io.FileUtils
import org.enso.languageserver.protocol.binary.{
  InboundPayload,
  OutboundMessage,
  OutboundPayload
}
import org.enso.languageserver.protocol.binary.FileContentsReply
import org.enso.languageserver.websocket.binary.factory.{
  InboundMessageFactory,
  PathFactory,
  ReadFileCommandFactory,
  WriteFileCommandFactory
}

import scala.io.Source

class BinaryFileManipulationTest extends BaseBinaryServerTest {

  implicit private val decoder = OutboundMessageDecoder

  "A WriteFileCommand" must {

    "persist binary contents of a file" in {
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

  def createWriteFileCmdPacket(
    requestId: UUID,
    pathSegment: String,
    rootId: UUID,
    contents: Array[Byte]
  ): ByteBuffer = {
    implicit val builder = new FlatBufferBuilder(1024)

    val path = PathFactory.create(rootId, Seq(pathSegment))

    val cmd = WriteFileCommandFactory.create(path, contents)

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
    implicit val builder = new FlatBufferBuilder(1024)

    val path = PathFactory.create(rootId, Seq(pathSegment))

    val cmd = ReadFileCommandFactory.create(path)

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
