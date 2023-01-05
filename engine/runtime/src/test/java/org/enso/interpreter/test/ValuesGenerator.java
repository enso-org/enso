package org.enso.interpreter.test;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
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
  private final Map<String, ValueInfo> values = new HashMap<>();

  private ValuesGenerator(Context ctx, Set<Language> languages) {
    this.ctx = ctx;
    this.languages = languages;
  }

  private record ValueInfo(Value type, Value check) {
  }

  public static ValuesGenerator create(Context ctx, Language... langs) {
    var set = langs == null || langs.length == 0 ? EnumSet.allOf(Language.class)
      : EnumSet.copyOf(Arrays.asList(langs));
    return new ValuesGenerator(ctx, set);
  }

  private ValueInfo v(String k, String t, String s) {
    var v = values.get(k);
    if (v == null) {
      var f = ctx.eval("enso", t + "\nn = " + s);
      var value = f.invokeMember("eval_expression", "n");
      var c = ctx.eval("enso", """
      {import}

      check x = case x of
          v : {type} -> 1
          _ -> 0

      """.replace("{type}", s).replace("{import}", t)
      );
      if (k != null) {
        var check = c.invokeMember("eval_expression", "check");
        assertTrue("Can execute the check", check.canExecute());
        v = new ValueInfo(value, check);
        values.put(k, v);
      } else {
        v = new ValueInfo(value, null);
      }
    }
    return v;
  }

  public Value typeAny() {
    return v("typeAny", """
    import Standard.Base.Any.Any
    """, "Any").type();
  }

  public Value typeNothing() {
    return v("typeNothing", """
    import Standard.Base.Nothing.Nothing
    """, "Nothing").type();
  }

  public Value typeNumber() {
    return v("typeNumber", """
    from Standard.Base import Nothing, Vector, Number, Decimal, Integer
    """, "Number").type();
  }

  public Value typeInteger() {
    return v("typeInteger", """
    from Standard.Base import Nothing, Vector, Number, Decimal, Integer
    """, "Integer").type();
  }

  public Value typeDecimal() {
    return v("typeDecimal", """
    from Standard.Base import Nothing, Vector, Number, Decimal, Integer
    """, "Decimal").type();
  }

  public Value typeBoolean() {
    return v("typeBoolean", """
    import Standard.Base.Data.Boolean.Boolean
    """, "Boolean").type();
  }

  public Value typeArrayProxy() {
    return v("typeArrayProxy", """
    import Standard.Base.Data.Array_Proxy.Array_Proxy
    """, "Array_Proxy").type();
  }

  public Value typeText() {
    return v("typeText", """
    import Standard.Base.Data.Text.Text
    """, "Text").type();
  }

  public Value typeDate() {
    return v("typeDate", """
    import Standard.Base.Data.Time.Date.Date
    """, "Date").type();
  }

  public Value typeDatePeriod() {
    return v("typeDate_Period", """
    import Standard.Base.Data.Time.Date_Period.Date_Period
    """, "Date_Period").type();
  }

  public Value typeDateTime() {
    return v("typeDate_Time", """
    import Standard.Base.Data.Time.Date_Time.Date_Time
    """, "Date_Time").type();
  }

  public Value typeTimeOfDay() {
    return v("typeTimeOfDay", """
    import Standard.Base.Data.Time.Time_Of_Day.Time_Of_Day
    """, "Time_Of_Day").type();
  }

  public Value typeDuration() {
    return v("typeDuration", """
    import Standard.Base.Data.Time.Duration.Duration
    """, "Duration").type();
  }

  public Value typePeriod() {
    return v("typePeriod", """
    import Standard.Base.Data.Time.Period.Period
    """, "Period").type();
  }

  public Value typeTimePeriod() {
    return v("typeTimePeriod", """
    import Standard.Base.Data.Time.Time_Period.Time_Period
    """, "Time_Period").type();
  }

  public Value typeTimeZone() {
    return v("typeTimeZone", """
    import Standard.Base.Data.Time.Time_Zone.Time_Zone
    """, "Time_Zone").type();
  }

  public Value typeArray() {
    return v("typeArray", """
    import Standard.Base.Data.Array.Array
    """, "Array").type();
  }

  public Value typeVector() {
    return v("typeVector", """
    import Standard.Base.Data.Vector.Vector
    """, "Vector").type();
  }

  public Value typeWarning() {
    return v("typeWarning", """
    import Standard.Base.Warning.Warning
    """, "Warning").type();
  }

  public Value typeFile() {
    return v("typeFile", """
    import Standard.Base.System.File.File
    """, "File").type();
  }

  public Value typeRef() {
    return v("typeRef", """
    import Standard.Base.Runtime.Ref.Ref
    """, "Ref").type();
  }

  public Value typeFunction() {
    return v("typeFunction", """
    import Standard.Base.Function.Function
    """, "Function").type();
  }

  public Value typeError() {
    return v("typeError", """
    import Standard.Base.Error.Error
    """, "Error").type();
  }

  public Value typePanic() {
    return v("typePanic", """
    import Standard.Base.Panic.Panic
    """, "Panic").type();
  }

  public Value typeManagedResource() {
    return v("typeManaged_Resource", """
    import Standard.Base.Runtime.Managed_Resource.Managed_Resource
    """, "Managed_Resource").type();
  }

  public Value withType(Value type) {
    for (var info : values.values()) {
      if (info.type() == type) {
        return info.check();
      }
    }
    throw new IllegalArgumentException("No with check for type " + type);
  }

  public List<Value> numbers() {
    var collect = new ArrayList<Value>();
    if (languages.contains(Language.ENSO)) {
      collect.add(v(null, "", "42").type());
      collect.add(v(null, "", "6.7").type());
      collect.add(v(null, "", "40321 * 43202").type());
      collect.add(v(null, """
      fac s n = if n <= 1 then s else
          @Tail_Call fac n*s n-1
      """, "fac 1 100").type());

    }

    if (languages.contains(Language.JAVA)) {
      collect.add(ctx.asValue((byte) 33));
      collect.add(ctx.asValue((short) 44));
      collect.add(ctx.asValue((int) 5432));
      collect.add(ctx.asValue((long) 5435432));
      collect.add(ctx.asValue((float) Math.PI));
      collect.add(ctx.asValue((double) Math.E));
    }

    for (var v : collect) {
      assertTrue("It's a number" + v, v.isNumber());
    }
    return collect;
  }

  public List<Value> textual() {
    var collect = new ArrayList<Value>();
    if (languages.contains(Language.ENSO)) {
      collect.add(v(null, "", "'fourty two'").type());
      collect.add(v(null, "", "'?'").type());
      collect.add(v(null, "", """
      '''
      block of
      multi-line
      texts
      """).type());
    }

    if (languages.contains(Language.JAVA)) {
      collect.add(ctx.asValue("fourty four from Java"));
      // collect.add(ctx.asValue('J'));
    }

    for (var v : collect) {
      assertFalse("It's not a number" + v, v.isNumber());
      assertTrue("It is a string" + v, v.isString());
    }
    return collect;
  }

  public List<Value> booleans() {
    var collect = new ArrayList<Value>();
    if (languages.contains(Language.ENSO)) {
      collect.add(v(null, "from Standard.Base.Data.Boolean.Boolean import True, False", "True").type());
      collect.add(v(null, "from Standard.Base.Data.Boolean.Boolean import True, False", "False").type());
    }

    if (languages.contains(Language.JAVA)) {
      collect.add(ctx.asValue(true));
      collect.add(ctx.asValue(false));
    }

    for (var v : collect) {
      assertTrue("It is an boolean" + v, v.isBoolean());
    }
    return collect;
  }

  public List<Value> times() {
    var collect = new ArrayList<Value>();
    if (languages.contains(Language.ENSO)) {
      collect.add(v(null, "import Standard.Base.Data.Time.Date.Date", "Date.now").type());
      collect.add(v(null, "import Standard.Base.Data.Time.Date_Time.Date_Time", "Date_Time.now").type());
      collect.add(v(null, "import Standard.Base.Data.Time.Time_Zone.Time_Zone", "Time_Zone.new").type());
      collect.add(v(null, "import Standard.Base.Data.Time.Time_Of_Day.Time_Of_Day", "Time_Of_Day.now").type());
      collect.add(v(null, "import Standard.Base.Data.Time.Duration.Duration", "Duration.new").type());
      for (var v : collect) {
        assertTrue("It is a time like value " + v, v.isDate() || v.isTime() || v.isTimeZone() || v.isDuration());
      }
      collect.add(v(null, "import Standard.Base.Data.Time.Date_Period.Date_Period", "Date_Period.Year").type());
      collect.add(v(null, "import Standard.Base.Data.Time.Time_Period.Time_Period", "Time_Period.Day").type());
      collect.add(v(null, "import Standard.Base.Data.Time.Period.Period", "Period.new 1 2 3").type());
    }

    if (languages.contains(Language.JAVA)) {
      collect.add(ctx.asValue(LocalDate.of(2022, 12, 10)));
      collect.add(ctx.asValue(LocalTime.of(12, 35)));
    }

    return collect;
  }

  public List<Value> arrayLike() {
    var collect = new ArrayList<Value>();
    if (languages.contains(Language.ENSO)) {
      collect.add(v(null, "", "[1, 2, 3]").type());
      collect.add(v(null, "", "['a', 'b']").type());
      collect.add(v(null, "", "[]").type());
      collect.add(v(null, "", "[1, 2, 3].to_array").type());
      collect.add(v(null, "", "['a', 'b'].to_array").type());
      collect.add(v(null, "", "[].to_array").type());
      collect.add(v(null, """
      import Standard.Base.Data.Array_Proxy.Array_Proxy
      """, "Array_Proxy.new 10 (x -> 2 * x)").type());
    }

    if (languages.contains(Language.JAVA)) {
      collect.add(ctx.asValue(new String[] { "Hello", "World!" }));
      collect.add(ctx.asValue(new int[] { 6, 7, 42 }));
      collect.add(ctx.asValue(List.of(1, 2, 3)));
    }

    for (var v : collect) {
      assertTrue("It is an array" + v, v.hasArrayElements());
    }
    return collect;
  }

  public List<Value> functions() {
    var collect = new ArrayList<Value>();
    if (languages.contains(Language.ENSO)) {
      collect.add(v(null, "mul x = x * 2", "mul").type());
    }

    if (languages.contains(Language.JAVA)) {
    }

    for (var v : collect) {
      assertTrue("It is can be executed" + v, v.canExecute());
    }
    return collect;
  }

  public List<Value> runtimeSystems() {
    var collect = new ArrayList<Value>();
    if (languages.contains(Language.ENSO)) {
      collect.add(v(null, "import Standard.Base.Runtime.Ref.Ref", "Ref.new 10").type());
      collect.add(v(null, "import Standard.Base.System.File.File", "File.new '/'").type());
      collect.add(v(null, "import Standard.Base.Runtime.Managed_Resource.Managed_Resource", "Managed_Resource.register '/' (x -> x)").type());
      collect.add(typeNothing());
    }

    if (languages.contains(Language.JAVA)) {
    }

    return collect;
  }

  public List<Value> errors() {
    var collect = new ArrayList<Value>();
    if (languages.contains(Language.ENSO)) {
      collect.add(v(null, """
      import Standard.Base.Any.Any
      import Standard.Base.Error.Error
      """, "Error.throw 'In error'").type());

      try {
        var noValue = v(null, """
        import Standard.Base.Any.Any
        import Standard.Base.Panic.Panic
        """, "Panic.throw 'In panic'").type();
        assertNull("Exception thrown instead", noValue);
      } catch (PolyglotException ex) {
        var panic = ex.getGuestObject();
        assertTrue("Is exception", panic.isException());
        collect.add(panic);
      }
    }

    if (languages.contains(Language.JAVA)) {
      collect.add(ctx.asValue(new IllegalStateException("In exception")));
    }

    return collect;
  }

  public List<Value> warnings() {
    var collect = new ArrayList<Value>();
    if (languages.contains(Language.ENSO)) {
      collect.add(v(null, """
      import Standard.Base.Warning.Warning
      """, "Warning.attach 'err' 'value'").type());
    }

    if (languages.contains(Language.JAVA)) {
    }

    return collect;
  }

  public List<Value> allValues() throws Exception {
    var collect = new ArrayList<Value>();
    for (var m : getClass().getMethods()) {
      if (m.getName().startsWith("all")) {
        continue;
      }
      if (m.getReturnType() == List.class) {
        @SuppressWarnings("unchecked")
        var r = (List<Value>) m.invoke(this);
        collect.addAll(r);
      }
    }
    return collect;
  }

  public List<Value> allTypes() throws Exception {
    var collect = new ArrayList<Value>();
    for (var m : getClass().getMethods()) {
      if (m.getName().startsWith("type")) {
        if (m.getReturnType() == Value.class) {
          var r = (Value) m.invoke(this);
          collect.add(r);
        }
      }
    }
    return collect;
  }

  public enum Language {
    ENSO, JAVASCRIPT, PYTHON, JAVA
  }
}
