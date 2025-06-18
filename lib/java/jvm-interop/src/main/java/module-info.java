import org.enso.common.PolyglotSymbolResolver;
import org.enso.jvm.interop.OtherJvmSymbolResolver;

module org.enso.jvm.interop {
  requires org.enso.engine.common;

  provides PolyglotSymbolResolver with
      OtherJvmSymbolResolver;

  requires org.graalvm.polyglot;
  requires org.graalvm.truffle;
  requires org.enso.jvm.channel;

  opens org.enso.jvm.interop to
      org.enso.jvm.channel;

  requires org.enso.persistance;
  requires static org.openide.util.lookup.RELEASE180;
}
