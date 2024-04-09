package org.enso.text

import java.nio.charset.StandardCharsets

import java.security.MessageDigest

/** SHA3-224 digest calculator. */
object Sha3_224VersionCalculator extends ContentBasedVersioning {

  /** @inheritdoc */
  override def evalVersion(content: CharSequence): ContentVersion = {
    val digestSHA3 = MessageDigest.getInstance("SHA3-224")
    val digest =
      digestSHA3.digest(content.toString.getBytes(StandardCharsets.UTF_8))
    ContentVersion(digest)
  }
}
