package org.enso.text

import java.nio.charset.StandardCharsets

import org.bouncycastle.jcajce.provider.digest.SHA3

/** SHA3-224 digest calculator. */
object Sha3_224VersionCalculator extends ContentBasedVersioning {

  /** @inheritdoc */
  override def evalVersion(content: CharSequence): ContentVersion = {
    val digestSHA3 = new SHA3.Digest224()
    val digest =
      digestSHA3.digest(content.toString.getBytes(StandardCharsets.UTF_8))
    ContentVersion(digest)
  }
}
