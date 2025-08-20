package org.enso.logging.service.common

import com.github.plokhotnyuk.jsoniter_scala.core.{
  readFromString,
  writeToString,
  JsonValueCodec
}
import com.github.plokhotnyuk.jsoniter_scala.macros.{
  named,
  CodecMakerConfig,
  JsonCodecMaker
}

object RefreshTokenPayload {
  implicit val payloadCodec: JsonValueCodec[Payload] =
    JsonCodecMaker.make[Payload](CodecMakerConfig.withAllowRecursiveTypes(true))

  implicit val authParametersCodec: JsonValueCodec[AuthParameters] =
    JsonCodecMaker.make[AuthParameters](
      CodecMakerConfig.withAllowRecursiveTypes(true)
    )

  implicit val authenticationResultCodec: JsonValueCodec[AuthenticationResult] =
    JsonCodecMaker.make[AuthenticationResult](
      CodecMakerConfig.withAllowRecursiveTypes(true)
    )

  implicit val responseCodec: JsonValueCodec[Response] =
    JsonCodecMaker.make[Response](
      CodecMakerConfig.withAllowRecursiveTypes(true)
    )

  def createRefreshTokenRequest(
    refreshToken: String,
    clientId: String
  ): String = {
    val payload = Payload(
      clientId = clientId,
      authFlow = "REFRESH_TOKEN_AUTH",
      authParameters = AuthParameters(
        refreshToken = refreshToken,
        deviceKey    = None
      )
    )
    writeToString(payload)
  }

  def decodeResponse(
    resp: String
  ): Response = {
    readFromString[Response](resp)
  }

  case class AuthParameters(
    @named("REFRESH_TOKEN")
    refreshToken: String,
    @named("DEVICE_KEY")
    deviceKey: Option[String]
  )

  case class Payload(
    @named("ClientId")
    clientId: String,
    @named("AuthFlow")
    authFlow: String,
    @named("AuthParameters")
    authParameters: AuthParameters
  )

  case class Response(
    @named("AuthenticationResult")
    authenticationResult: AuthenticationResult
  )

  /** @param accessToken
    * @param tokenType
    * @param expiresIn Expiration in seconds
    */
  case class AuthenticationResult(
    @named("AccessToken")
    accessToken: String,
    @named("TokenType")
    tokenType: String,
    @named("ExpiresIn")
    expiresIn: Int
  )
}
