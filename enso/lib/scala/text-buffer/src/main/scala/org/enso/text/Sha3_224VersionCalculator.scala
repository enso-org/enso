package org.enso.text

import java.nio.charset.StandardCharsets

import org.bouncycastle.jcajce.provider.digest.SHA3

/** SHA3-224 digest calculator. */
object Sha3_224VersionCalculator extends ContentBasedVersioning {

  /** @inheritdoc */
  override def evalVersion(content: String): ContentVersion = {
    val digestSHA3 = new SHA3.Digest224()
    ContentVersion(digestSHA3.digest(content.getBytes(StandardCharsets.UTF_8)))
  }

}
