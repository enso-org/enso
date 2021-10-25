package org.enso.text

import org.bouncycastle.util.encoders.Hex

/** Version of the text contents. */
case class ContentVersion(toHexString: String) {

  /** Calculate digest. */
  def toDigest: Array[Byte] =
    Hex.decode(toHexString)
}

object ContentVersion {

  /** Create [[ContentVersion]] from digest.
    *
    * @param digest the version digest
    * @return new content version
    */
  def apply(digest: Array[Byte]): ContentVersion =
    ContentVersion(Hex.toHexString(digest))
}
