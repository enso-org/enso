package org.enso.interpreter.runtime;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.source.Source;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.LanguageInfo;
import org.enso.text.buffer.Rope;


/** Keeps information about various kinds of sources associated with a {@link Module}.
 */
record ModuleSources(
          TruffleFile sourceFile,
          Rope ropeHolder,
          Source cachedSource) {

  static final ModuleSources NONE = new ModuleSources(null, null, null);

  ModuleSources newWith(TruffleFile f) {
    return new ModuleSources(f, ropeHolder(), cachedSource());
  }

  ModuleSources newWith(Rope r) {
    return new ModuleSources(sourceFile(), r, null);
  }

  ModuleSources ensureCachedSource(QualifiedName name) throws IOException {
    if (cachedSource != null) {
      return this;
    }
    URI uri;
    try {
      uri = new URI("enso://" + Integer.toHexString(System.identityHashCode(this)) + "/" + name);
    } catch (URISyntaxException ex) {
      throw new IllegalStateException(ex);
    }
    if (literalSource() != null) {
      com.oracle.truffle.api.source.Source src = Source.newBuilder(LanguageInfo.ID, ropeHolder.characters(), name.toString()).uri(uri).build();
      return new ModuleSources(sourceFile, ropeHolder, src);
    } else if (sourceFile != null) {
      com.oracle.truffle.api.source.Source src = Source.newBuilder(LanguageInfo.ID, sourceFile).uri(uri).build();
      org.enso.text.buffer.Rope lit = Rope.apply(src.getCharacters().toString());
      return new ModuleSources(sourceFile, lit, src);
    }
    throw new IllegalStateException();
  }

  String getPath() {
    return sourceFile() == null ? null : sourceFile().getPath();
  }

  Rope literalSource() {
    return ropeHolder;
  }
}
