package org.enso.test.utils;

import org.enso.pkg.QualifiedName;

/**
 * A simple structure corresponding to an Enso module.
 */
public record SourceModule(QualifiedName name, String code) {

}
