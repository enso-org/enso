package org.enso.docs

import org.enso.syntax.text.ast.Doc

package object sections {

  type ParsedSection   = Section[Doc.Elem]
  type RenderedSection = Section[String]
}
