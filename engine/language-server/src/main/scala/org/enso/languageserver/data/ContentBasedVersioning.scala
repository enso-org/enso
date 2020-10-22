package org.enso.languageserver.data

/** A content-based versioning calculator.
  */
trait ContentBasedVersioning {

  /** Evaluates content-based version of document.
    *
    * @param content a textual content
    * @return a string representation of a digest
    */
  def evalVersion(content: String): String

  /** Evaluates content-based version of document.
    *
    * @param content a textual content
    * @return a digest
    */
  def evalDigest(content: String): Array[Byte]

}
