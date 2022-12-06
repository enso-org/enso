package org.enso.interpreter.test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.Assert;
import static org.junit.Assert.assertTrue;

/**
 * The purpose of this class is to generate various values needed for other
 * tests. Users of this support class can configure what kind of values they
 * need - Enso, Java, JavaScript, etc. and then call appropriate methods to
 * obtain such values. It's up to the tests to use these values meaningfully.
 */
class ValuesGenerator {
  private final Context ctx;
  private final Set<Language> languages;
  private final Map<String, Value> values = new HashMap<>();

  private ValuesGenerator(Context ctx, Set<Language> languages) {
    this.ctx = ctx;
    this.languages = languages;
  }


  public static ValuesGenerator create(Context ctx, Language... langs) {
    return new ValuesGenerator(ctx, EnumSet.copyOf(Arrays.asList(langs)));
  }

  private Value v(String k, String t, String s) {
    var v = values.get(k);
    if (v == null) {
      v = ctx.eval("enso", t + "\nn = " + s).invokeMember("eval_expression", s);
      if (k != null) {
        values.put(k, v);
      }
    }
    Assert.assertFalse("Not a function", v.canExecute());
    return v;
  }

  public Value typeAny() {
    return v("typeAny", """
    import Standard.Base.Any.Any
    """, "Any");
  }

  public Value typeNumber() {
    return v("typeNumber", """
    from Standard.Base import Nothing, Vector, Number, Decimal, Integer
    """, "Number");
  }

  public List<Value> numbers() {
    var collect = new ArrayList<Value>();
    if (languages.contains(Language.ENSO)) {
      collect.add(v(null, "", "42"));
      collect.add(v(null, "", "6.7"));
      collect.add(v(null, "", "40321 * 43202"));
      collect.add(v(null, """
      fac s n = if n <= 1 then s else
          @Tail_Call fac n*s n-1
      """, "fac 1 100"));

    }

    for (var v : collect) {
      assertTrue("It's a number" + v, v.isNumber());
    }
    return collect;
  }

  public enum Language {
    ENSO, JAVASCRIPT, PYTHON, JAVA
  }
}
