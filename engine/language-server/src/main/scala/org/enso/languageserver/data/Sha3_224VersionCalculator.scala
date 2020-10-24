package org.enso.languageserver.data

import org.bouncycastle.jcajce.provider.digest.SHA3
import org.bouncycastle.util.encoders.Hex

/** SHA3-224 digest calculator.
  */
object Sha3_224VersionCalculator extends ContentBasedVersioning {

  /** @inheritdoc */
  override def evalVersion(content: String): String =
    Hex.toHexString(evalDigest(content))

  /** @inheritdoc */
  override def evalDigest(content: String): Array[Byte] = {
    val digestSHA3 = new SHA3.Digest224()
    digestSHA3.digest(content.getBytes("UTF-8"))
  }

}
