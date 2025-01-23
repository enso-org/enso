package org.enso.interpreter.runtime;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.source.Source;
import java.io.IOException;
import java.util.LinkedList;
import org.enso.common.LanguageInfo;
import org.enso.pkg.QualifiedName;
import org.enso.text.buffer.Rope;
import org.enso.version.BuildVersion;

/**
 * Keeps information about various kinds of sources associated with a {@link Module}. All the record
 * fields are immutable. They can only change by creating new instance of the whole record - usually
 * via methods {@link #newWith(org.enso.text.buffer.Rope)} or {@link
 * #newWith(com.oracle.truffle.api.TruffleFile)}. The creation of cached Truffle {@link Source} is
 * delayed and happens only per {@link #ensureCachedSource(org.enso.pkg.QualifiedName) request}.
 */
record ModuleSources(TruffleFile file, Rope rope, Source source) {

  /** Empty triple of sources. Continue with {@code newWith} methods. */
  static final ModuleSources NONE = new ModuleSources(null, null, null);

  /**
   * Creates sources instances associated with provided Truffle file.
   *
   * @param f the file to associate the sources with
   * @return new sources triple
   */
  ModuleSources newWith(TruffleFile f) {
    return new ModuleSources(f, null, null);
  }

  /**
   * Associates current sources with modified text content. The {@code file} is kept unchanged. The
   * cached {@code source} is cleared.
   *
   * @param r the new text content to assign to the sources
   * @return new sources triple
   */
  ModuleSources newWith(Rope r) {
    return new ModuleSources(file(), r, null);
  }

  /**
   * Resets the contents of sources but retains the reference to the file. Continue with {@code
   * newWith} methods.
   *
   * @return new sources triple
   */
  ModuleSources reset() {
    return new ModuleSources(file(), null, null);
  }

  /**
   * Makes sure Truffle {@code Source} object is ready for the sources. If this triple already
   * contains Truffle source, it just returns itself. Otherwise, if the {@link #rope() content} is
   * set, it is used as chars for the Truffle source. If the file isn't in memory yet, it is read in
   * and both {@link #source()} and {@link #rope()} are initialized.
   *
   * @param name the name of the associated module
   * @return the same or new triple of sources
   * @throws IOException exception if loading fails
   */
  ModuleSources ensureSource(QualifiedName name) throws IOException {
    if (source != null) {
      return this;
    }
    if (rope() != null) {
      Source src = Source.newBuilder(LanguageInfo.ID, rope.characters(), name.toString()).build();
      return new ModuleSources(file, rope, src);
    } else if (file != null) {
      var b = Source.newBuilder(LanguageInfo.ID, file);
      alternativeFile(b, file);
      var src = b.build();
      org.enso.text.buffer.Rope lit = Rope.apply(src.getCharacters().toString());
      return new ModuleSources(file, lit, src);
    }
    throw new IllegalStateException();
  }

  /**
   * Path of the associated {@link #file()}.
   *
   * @return path or {@code null}
   */
  String getPath() {
    return file() == null ? null : file().getPath();
  }

  /**
   * Special support for reassigning locations of sources when running development version. When
   * {@code -ea} is enabled, then we try to locate library files under the {@code
   * distribution}/{@code lib} directories.
   */
  private static void alternativeFile(Source.SourceBuilder b, TruffleFile file) {
    if (!BuildVersion.defaultDevEnsoVersion().equals(BuildVersion.ensoVersion())) {
      // no changes when not running development version
      return;
    }
    var root = file;
    var stack = new LinkedList<String>();
    while (root != null) {
      if (BuildVersion.defaultDevEnsoVersion().equals(root.getName())) {
        break;
      }
      stack.addFirst(root.getName());
      root = root.getParent();
    }
    if (root == null) {
      // give up when we cannot find a root
      return;
    }
    var libName = root.getParent();
    if (libName == null) {
      return;
    }
    var libNamespace = libName.getParent();
    if (libNamespace == null) {
      return;
    }
    var libVersion = libNamespace.getParent().getParent();
    if (libVersion == null || libVersion.getParent() == null) {
      return;
    }
    var builtDistribution = libVersion.getParent().getParent();
    if ("built-distribution".equals(builtDistribution.getName())) {
      var repositoryRoot = builtDistribution.getParent();
      var distRoot = repositoryRoot.resolve("distribution").resolve("lib");
      var newLibRoot =
          distRoot
              .resolve(libNamespace.getName())
              .resolve(libName.getName())
              .resolve(root.getName());
      if (newLibRoot.isDirectory()) {
        var newFile = newLibRoot;
        for (var n : stack) {
          newFile = newFile.resolve(n);
        }
        if (newFile.exists()) {
          b.uri(newFile.toUri());
        }
      }
    }
  }
}
