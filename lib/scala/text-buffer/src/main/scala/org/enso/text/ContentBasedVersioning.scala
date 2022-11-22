package org.enso.text

/** A content-based versioning calculator. */
trait ContentBasedVersioning {

  /** Evaluates content-based version of document.
    *
    * @param content a textual content
    * @return a content version
    */
  def evalVersion(content: CharSequence): ContentVersion
}
