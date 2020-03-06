package org.enso.languageserver.data

import org.bouncycastle.jcajce.provider.digest.SHA3
import org.bouncycastle.util.encoders.Hex

/**
  * SHA3-224 digest calculator.
  */
object Sha3_224VersionCalculator extends ContentBasedVersioning {

  /**
    * Digests textual content.
    *
    * @param content a textual content
    * @return a digest
    */
  override def evalVersion(content: String): String = {
    val digestSHA3 = new SHA3.Digest224()
    val hash       = digestSHA3.digest(content.getBytes("UTF-8"))
    Hex.toHexString(hash)
  }

}
