package org.enso.logging.service.common

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

import java.io.File
import java.nio.file.Files

case class Credentials(
  @named("client_id")
  clientId: String,
  @named("access_token")
  accessToken: String,
  @named("refresh_token")
  refreshToken: String,
  @named("refresh_url")
  refreshUrl: String,
  @named("expire_at")
  expireAt: String
) {
  override def toString: String = {
    s"Credentials{clientId: ${substr(clientId)}, " +
    s"accessToken: ${substr(accessToken)}, " +
    s"refreshToken: ${substr(refreshToken)}, " +
    s"refreshUrl: $refreshUrl, " +
    s"expireAt: $expireAt}"
  }

  private def substr(str: String): String = {
    str.substring(0, Math.min(str.length, 8))
  }
}

object Credentials {
  implicit val fileCodec: JsonValueCodec[Credentials] = {
    JsonCodecMaker.make[Credentials](
      CodecMakerConfig.withAllowRecursiveTypes(true)
    )
  }

  def parseFromFile(file: File): Credentials = {
    val bytes = Files.readAllBytes(file.toPath)
    readFromArray[Credentials](bytes)
  }
}
