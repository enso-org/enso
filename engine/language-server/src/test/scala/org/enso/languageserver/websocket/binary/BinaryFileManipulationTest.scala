package org.enso.languageserver.websocket.binary

import com.google.flatbuffers.FlatBufferBuilder
import org.apache.commons.io.FileUtils
import org.enso.languageserver.protocol.binary._
import org.enso.languageserver.websocket.binary.factory._
import org.enso.testkit.FlakySpec

import java.io.File
import java.nio.ByteBuffer
import java.nio.file.Files
import java.security.MessageDigest
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
      val requestId = UUID.randomUUID()
      val filename  = "bar.bin"
      val barFile   = new File(testContentRoot.toFile, filename)
      val contents  = Array[Byte](65, 66, 67) //ABC
      FileUtils.writeByteArrayToFile(barFile, contents)

      val expectedChecksum = ByteBuffer.wrap(
        MessageDigest.getInstance("SHA3-224").digest(contents.slice(0, 2))
      )

      val client = newWsClient()
      client.send(createSessionInitCmd())
      client.expectFrame()
      val checksumBytesCmd = createChecksumBytesCommandPacket(
        requestId,
        Seq(filename),
        testContentRootId,
        byteOffset = 0,
        length     = 2
      )

      client.send(checksumBytesCmd)
      val checksumResponse = client
        .receiveMessage[OutboundMessage]()
        .getOrElse(fail("Should be right"))
      checksumResponse
        .payloadType() shouldEqual OutboundPayload.CHECKSUM_BYTES_REPLY
      val payload = checksumResponse
        .payload(new ChecksumBytesReply)
        .asInstanceOf[ChecksumBytesReply]
      val digest = payload.checksum().bytesAsByteBuffer()

      digest shouldEqual expectedChecksum

      Files.delete(barFile.toPath)
    }

    "Return a `FileNotFound` error if the file does not exist" in {
      val requestId = UUID.randomUUID()
      val filename  = "does_not_exist.bin"

      val client = newWsClient()
      client.send(createSessionInitCmd())
      client.expectFrame()
      val checksumBytesCmd = createChecksumBytesCommandPacket(
        requestId,
        Seq(filename),
        testContentRootId,
        byteOffset = 0,
        length     = 2
      )

      client.send(checksumBytesCmd)
      val checksumResponse = client
        .receiveMessage[OutboundMessage]()
        .getOrElse(fail("Should be right"))
      checksumResponse.payloadType() shouldEqual OutboundPayload.ERROR
      val payload = checksumResponse
        .payload(new Error)
        .asInstanceOf[Error]
      payload.code() shouldEqual 1003
      payload.message() shouldEqual "File not found"
    }

    "Return a `ReadOutOfBounds` error if the byte range is out of bounds" in {
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
        byteOffset = 3,
        length     = 2
      )

      client.send(checksumBytesCmd)
      val checksumResponse = client
        .receiveMessage[OutboundMessage]()
        .getOrElse(fail("Should be right"))
      checksumResponse.payloadType() shouldEqual OutboundPayload.ERROR
      val payload = checksumResponse
        .payload(new Error)
        .asInstanceOf[Error]
      payload.code() shouldEqual 1009
      payload.message() shouldEqual "Read is out of bounds for the file"
      payload.dataType() shouldEqual ErrorPayload.READ_OOB
      val data = payload
        .data(new ReadOutOfBoundsError)
        .asInstanceOf[ReadOutOfBoundsError]
      data.fileLength() shouldEqual 3
    }

    "Return a `NotFile` error if the provided path is not a file" in {
      val requestId = UUID.randomUUID()

      val client = newWsClient()
      client.send(createSessionInitCmd())
      client.expectFrame()
      val checksumBytesCmd = createChecksumBytesCommandPacket(
        requestId,
        Seq(),
        testContentRootId,
        byteOffset = 3,
        length     = 2
      )

      client.send(checksumBytesCmd)
      val checksumResponse = client
        .receiveMessage[OutboundMessage]()
        .getOrElse(fail("Should be right"))
      checksumResponse.payloadType() shouldEqual OutboundPayload.ERROR
      val payload = checksumResponse
        .payload(new Error)
        .asInstanceOf[Error]
      payload.code() shouldEqual 1007
      payload.message() shouldEqual "Path is not a file"
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
