package org.enso.interpreter.epb;

import com.oracle.truffle.api.instrumentation.TruffleInstrument;
import com.oracle.truffle.api.source.Source;
import java.io.IOException;
import java.util.function.BiFunction;

@TruffleInstrument.Registration(id = EpbParseInstrument.ID, services = BiFunction.class)
public final class EpbParseInstrument extends TruffleInstrument {
  static final String ID = "epbParseInstrument";

  @Override
  protected void onCreate(Env env) {
    BiFunction<String, String, Object> fn =
        (code, name) -> {
          var src = Source.newBuilder(EpbLanguage.ID, code, name).build();
          try {
            var target = env.parse(src);
            return target.call();
          } catch (IOException ex) {
            throw new IllegalStateException(ex);
          }
        };
    env.registerService(fn);
  }
}
