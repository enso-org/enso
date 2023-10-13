package org.enso.jsonrpc

import com.typesafe.config.Config

import java.io.{ByteArrayInputStream, File, FileInputStream, InputStream}
import java.security.cert.{CertificateFactory, X509Certificate}
import java.security.spec.PKCS8EncodedKeySpec
import java.security.{KeyFactory, KeyStore, SecureRandom}
import java.util.Base64
import javax.net.ssl.{
  KeyManagerFactory,
  SSLContext,
  TrustManager,
  X509TrustManager
}
import scala.util.Try

/** Base class for generating custom {@link SSLContext} from configs.
  *
  * @param trustSelfSignedCerts true, if the SLLContext should trust all self-signed certificates
  */
abstract class SecureConnectionConfig(trustSelfSignedCerts: Boolean) {
  def generateSSLContext(): Try[SSLContext]

  protected def trustManagers: Array[TrustManager] = {
    if (trustSelfSignedCerts) {
      Array[TrustManager](new X509TrustManager {
        override def checkClientTrusted(
          chain: Array[X509Certificate],
          authType: String
        ): Unit = {}

        override def checkServerTrusted(
          chain: Array[X509Certificate],
          authType: String
        ): Unit = {}

        override def getAcceptedIssuers: Array[X509Certificate] = new Array(0)
      })
    } else {
      null
    }
  }
}

object SecureConnectionConfig {

  /** Infers secure configuration from application's config.
    * If the config has a `pkcs12-file` key, the configuration will be for the PKCS12-formatted certificate.
    * If the config has a `public-certificate` key, the configuration will be inferred for the public/public certificate/key.
    * If none of the above, returns a failure since no secure configuration is present in the application's config.
    *
    * @param config application.conf config
    * @return left value with a failure or right value with a validated secure configuration
    */
  def fromApplicationConfig(
    config: Config
  ): Either[Option[String], SecureConnectionConfig] = {
    if (config.hasPath("pkcs12-file")) {
      (for {
        pkcs12 <- getStringFieldOpt(config, "pkcs12-file")
        pkcs12File = new File(pkcs12)
        password        <- getStringFieldOpt(config, "pkcs12-password")
        trustSelfSigned <- getBooleanFieldOpt(config, "trust-self-signed")
      } yield SecureConnectionConfigForPKCS12(pkcs12File, password)(
        trustSelfSigned
      )).left.map(Some(_))
    } else if (config.hasPath("public-certificate")) {
      (for {
        publicKeyCertificate <- getStringFieldOpt(config, "public-certificate")
        publicKeyCertificateAlg <- getStringFieldOpt(
          config,
          "public-certificate-algorithm"
        )
        privateKey      <- getStringFieldOpt(config, "private-key")
        trustSelfSigned <- getBooleanFieldOpt(config, "trust-self-signed")
      } yield SecureConnectionConfigForPublicPrivateCert(
        publicKeyCertificate,
        publicKeyCertificateAlg,
        privateKey
      )(trustSelfSigned)).left.map(Some(_))

    } else Left(None)
  }

  private def getStringFieldOpt(
    config: Config,
    fieldName: String
  ): Either[String, String] = {
    if (config.hasPath(fieldName)) {
      val v = config.getString(fieldName)
      if (v == null || v.isEmpty) Left(s"field $fieldName is empty")
      else Right(v)
    } else {
      Left(s"missing $fieldName")
    }
  }

  private def getBooleanFieldOpt(
    config: Config,
    fieldName: String
  ): Either[String, Boolean] = {
    if (config.hasPath(fieldName)) {
      Right(config.getBoolean(fieldName))
    } else {
      Left(s"missing $fieldName")
    }
  }

  /** Configuration for SSLContext from PKCS12 format with a corresponding private password.
    * Generation of the SSLContext will create a local Java keystore on-the-fly, based on the provided PKCS12 bundle.
    *
    * @param pkcsInputStream             input stream to PKCS12-formatted object
    * @param password                    password to a certificate
    * @param trustSelfSignedCertificates true, if all self-signed certificates should be trusted
    */
  case class SecureConnectionConfigForPKCS12(
    pkcsInputStream: InputStream,
    password: String
  )(trustSelfSignedCertificates: Boolean)
      extends SecureConnectionConfig(trustSelfSignedCertificates) {

    private val keystoreType               = "PKCS12"
    private val keyManagerFactoryAlgorithm = "SunX509"
    private val sslType                    = "TLS"

    def generateSSLContext(): Try[SSLContext] = Try {
      val keyStore      = KeyStore.getInstance(keystoreType)
      val passwordChars = password.toCharArray
      keyStore.load(pkcsInputStream, passwordChars)

      val kmf = KeyManagerFactory.getInstance(keyManagerFactoryAlgorithm)
      kmf.init(keyStore, passwordChars)
      val keyManagers = kmf.getKeyManagers
      val sslContext  = SSLContext.getInstance(sslType)
      sslContext.init(keyManagers, trustManagers, new SecureRandom())
      sslContext
    }
  }

  object SecureConnectionConfigForPKCS12 {
    def apply(
      pkcsFile: File,
      password: String
    )(trustSelfSignedCertificates: Boolean): SecureConnectionConfig =
      SecureConnectionConfigForPKCS12(
        new FileInputStream(pkcsFile),
        password
      )(trustSelfSignedCertificates)
  }

  /** Configuration for SSLContext from certificate and a corresponding private key.
    * Generation of the SSLContext will create a local Java keystore on-the-fly, based on the provided values.
    *
    * @param publicCertificate           contents of the certificate
    * @param publicCertificateAlgorithm  algorithm used in the certificate
    * @param privateKey                  private key
    * @param trustSelfSignedCertificates true, if all self-signed certificates should be trusted
    */
  case class SecureConnectionConfigForPublicPrivateCert(
    publicCertificate: String,
    publicCertificateAlgorithm: String,
    privateKey: String
  )(trustSelfSignedCertificates: Boolean)
      extends SecureConnectionConfig(trustSelfSignedCertificates) {

    private val beginPrivateHeader         = "-----BEGIN PRIVATE KEY-----"
    private val endPrivateSuffix           = "-----END PRIVATE KEY-----"
    private val targetKeyStore             = "PKCS12"
    private val publicPrivateAlg           = "RSA"
    private val sslType                    = "TLS"
    private val keyManagerFactoryAlgorithm = "SunX509"

    override def generateSSLContext(): Try[SSLContext] = Try {
      val factory = CertificateFactory.getInstance(publicCertificateAlgorithm)
      val certificateStream =
        new ByteArrayInputStream(publicCertificate.getBytes())
      val cert = factory.generateCertificate(certificateStream)

      val prefixIdx = privateKey.indexOf(beginPrivateHeader)
      val privateKeyWithDroppedAttributes =
        if (prefixIdx == -1) privateKey else privateKey.substring(prefixIdx)

      val privateKeyPEM = privateKeyWithDroppedAttributes
        .replaceAll("\\R", "")
        .replace(beginPrivateHeader, "")
        .replace(endPrivateSuffix, "")
      val privateKeyDER = Base64.getDecoder().decode(privateKeyPEM);

      val spec            = new PKCS8EncodedKeySpec(privateKeyDER);
      val keyFactory      = KeyFactory.getInstance(publicPrivateAlg);
      val storePrivateKey = keyFactory.generatePrivate(spec);
      val keyStore        = KeyStore.getInstance(targetKeyStore)
      keyStore.load(null)
      val password = "temp-keystore".toCharArray
      keyStore.setKeyEntry("enso", storePrivateKey, password, Array(cert))

      val kmf =
        KeyManagerFactory.getInstance(keyManagerFactoryAlgorithm)
      kmf.init(keyStore, password)
      val keyManagers = kmf.getKeyManagers
      val sslContext  = SSLContext.getInstance(sslType)

      sslContext.init(keyManagers, trustManagers, new SecureRandom())
      sslContext
    }
  }

}
