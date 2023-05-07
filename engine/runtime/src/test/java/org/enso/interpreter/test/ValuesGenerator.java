package org.enso.interpreter.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.Period;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import org.enso.polyglot.MethodNames.Module;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;

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
  private final Map<String, List<Value>> multiValues = new HashMap<>();

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

  private ValueInfo v(String key, String prelude, String typeOrValue) {
    return v(key, prelude, typeOrValue, key != null ? typeOrValue : null);
  }

  private ValueInfo v(String key, String prelude, String typeOrValue, String typeCheck) {
    if (key == null) {
      key = typeOrValue;
    }
    var v = values.get(key);
    if (v == null) {
      var code = prelude + "\nn = " + typeOrValue;
      var f = ctx.eval("enso", code);
      var value = f.invokeMember("eval_expression", "n");
      if (typeCheck != null) {
        var c = ctx.eval("enso", """
        {import}

        check x = case x of
            _ : {type} -> 1
            _ -> 0

        """.replace("{type}", typeCheck).replace("{import}", prelude)
        );
        var check = c.invokeMember("eval_expression", "check");
        assertTrue("Can execute the check", check.canExecute());
        v = new ValueInfo(value, check);
        values.put(key, v);
      } else {
        v = new ValueInfo(value, null);
      }
    }
    return v;
  }

  /**
   * Converts expressions into values of type described by {@code typeDefs} by concatenating
   * everything into a single source.
   *
   * This method exists so that there are no multiple definitions of a single type.
   *
   * @param typeDefs Type definitions.
   * @param expressions List of expressions - every expression will be converted to a {@link Value}.
   * @param checks list of names (with {@code null}) to define checks for
   * @return List of values converted from the given expressions.
   */
  private List<Value> createValuesOfCustomType(String typeDefs, List<String> expressions, List<String> checks) {
    var prev = multiValues.get(typeDefs);
    if (prev != null) {
      return prev;
    }

    var sb = new StringBuilder();
    sb.append(typeDefs);
    sb.append("\n");
    for (int i = 0; i < expressions.size(); i++) {
      sb.append("var_").append(i).append(" = ").append(expressions.get(i)).append("\n");
    }
    for (int i = 0; i < expressions.size(); i++) {
      var c = checks != null ? checks.get(i) : null;
      if (c == null) {
        continue;
      }
      sb.append("""
      check_{i} x = case x of
          _ : {type} -> 1
          _ -> 0

      """.replace("{type}", c).replace("{i}", "" + i));
    }
    Value module = ctx.eval("enso", sb.toString());
    List<Value> values = new ArrayList<>(expressions.size());
    for (int i = 0; i < expressions.size(); i++) {
      Value val = module.invokeMember(Module.EVAL_EXPRESSION, "var_" + i);
      values.add(val);
      var c = checks != null ? checks.get(i) : null;
      if (c == null) {
        continue;
      }
      Value check = module.invokeMember(Module.EVAL_EXPRESSION, "check_" + i);
      this.values.put(c, new ValueInfo(val, check));
    }
    multiValues.put(typeDefs, values);
    return values;
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

  public Value typeMap() {
    return v("typeMap", """
    import Standard.Base.Data.Map.Map
    """, "Map").type();
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
      collect.add(
          v(
                  null,
                  """
      from Standard.Base.Data.Ordering import all

      fac s n = if n <= 1 then s else
          @Tail_Call fac n*s n-1
      """,
                  "fac 1 100")
              .type());
      collect.add(v(null, "", "123 * 10^40").type());
      collect.add(v(null, "", "123 * 10^40 + 0.0").type());
      collect.add(v(null, "", "123 * 10^40 + 1.0").type());
      collect.add(v(null, "import Standard.Base.Data.Numbers.Number", "Number.nan").type());
    }

    if (languages.contains(Language.JAVA)) {
      collect.add(ctx.asValue((byte) 33));
      collect.add(ctx.asValue((short) 44));
      collect.add(ctx.asValue((int) 5432));
      collect.add(ctx.asValue((long) 5435432));
      collect.add(ctx.asValue((float) Math.PI));
      collect.add(ctx.asValue((double) Math.E));
      collect.add(ctx.asValue(Double.NaN));
    }

    for (var v : collect) {
      assertTrue("It's a number" + v, v.isNumber());
    }
    return collect;
  }

  public List<Value> textual() {
    var collect = new ArrayList<Value>();
    if (languages.contains(Language.ENSO)) {
      // TODO: Add once PR #3956 is merged
      //collect.add(v(null, "", "''").type());
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
      collect.add(ctx.asValue("♥"));
      collect.add(ctx.asValue("吰 abcde 1"));
      collect.add(ctx.asValue("1234"));
      collect.add(ctx.asValue("\t"));
      collect.add(ctx.asValue("\n"));
      collect.add(ctx.asValue("\r"));
      collect.add(ctx.asValue("\r\t \t\r"));
      collect.add(ctx.asValue("J"));
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

  public List<Value> timesAndDates() {
    var collect = new ArrayList<Value>();
    if (languages.contains(Language.ENSO)) {
      collect.add(v(null, "import Standard.Base.Data.Time.Date.Date", "Date.now").type());
      collect.add(v(null, "import Standard.Base.Data.Time.Date.Date", "Date.new 1999 3 23").type());
      collect.add(v(null, "import Standard.Base.Data.Time.Date_Time.Date_Time", "Date_Time.now").type());
      collect.add(v(null, "import Standard.Base.Data.Time.Date_Time.Date_Time", "Date_Time.parse '2021-01-01T00:30:12.7102[UTC]'").type());
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
      collect.add(ctx.asValue(LocalDate.of(1999, 3, 23)));
      collect.add(ctx.asValue(LocalTime.of(12, 35)));
      collect.add(ctx.asValue(ZonedDateTime.of(2021, 1, 1, 0, 30, 12, 710200000, ZoneId.of("Z"))));
    }

    return collect;
  }

  public List<Value> timeZones() {
    var collect = new ArrayList<Value>();
    if (languages.contains(Language.ENSO)) {
      for (var expr : List.of(
          "Time_Zone.new",
          "Time_Zone.system",
          "Time_Zone.local",
          "Time_Zone.utc",
          "Time_Zone.new 1 2 3",
          "Time_Zone.parse 'Europe/Moscow'",
          "Time_Zone.parse 'Europe/London'",
          "Time_Zone.parse 'CET'"
      )) {
        collect.add(v("timeZones-" + expr, "import Standard.Base.Data.Time.Time_Zone.Time_Zone", expr, "Time_Zone").type());
      }
    }
    if (languages.contains(Language.JAVA)) {
      for (var javaValue : List.of(
          TimeZone.getTimeZone("America/Los_Angeles"),
          TimeZone.getTimeZone(ZoneId.systemDefault()),
          TimeZone.getTimeZone(ZoneId.ofOffset("GMT", ZoneOffset.ofHours(2))),
          TimeZone.getTimeZone(ZoneId.ofOffset("GMT", ZoneOffset.ofHoursMinutes(14, 45))),
          TimeZone.getTimeZone(ZoneId.ofOffset("UTC", ZoneOffset.ofHours(-15)))
      )) {
        collect.add(ctx.asValue(javaValue));
      }
    }
    return collect;
  }

  public List<Value> durations() {
    var collect = new ArrayList<Value>();
    if (languages.contains(Language.ENSO)) {
      for (var expr : List.of(
          "Duration.zero",
          "Duration.new 1",
          "Duration.new 1 1",
          "Duration.new nanoseconds=900",
          "Duration.new minutes=900",
          "Duration.between (Date_Time.new 2022 01 01) (Date_Time.new 2022 02 02)",
          "Duration.between (Date_Time.new 2022 01 01) (Date_Time.new 2022 02 02) timezone_aware=False"
      )) {
        collect.add(v(null, """
                                  import Standard.Base.Data.Time.Duration.Duration
                                  import Standard.Base.Data.Time.Date_Time.Date_Time
                                  from Standard.Base.Data.Boolean.Boolean import False
                                  """, expr).type());
      }
    }
    if (languages.contains(Language.JAVA)) {
      for (var javaValue : List.of(
          Duration.ofHours(1),
          Duration.ofHours(0),
          Duration.ofSeconds(600),
          Duration.ofNanos(9784),
          Duration.ZERO
      )) {
        collect.add(ctx.asValue(javaValue));
      }
    }
    collect.forEach(value -> assertTrue("Is duration: " + value, value.isDuration()));
    return collect;
  }

  public List<Value> periods() {
    var collect = new ArrayList<Value>();
    if (languages.contains(Language.ENSO)) {
      for (var expr : List.of(
          "Period.new",
          "Period.new 1",
          "Period.new 1 14",
          "Period.new days=568",
          "Period.new years=23451"
      )) {
        collect.add(v(null, "import Standard.Base.Data.Time.Period.Period", expr).type());
      }
    }
    if (languages.contains(Language.JAVA)) {
      for (var javaValue : List.of(
          Period.ZERO,
          Period.ofDays(12),
          Period.ofDays(65),
          Period.ofMonths(13),
          Period.of(12, 4, 60),
          Period.ofYears(23410)
      )) {
        collect.add(ctx.asValue(javaValue));
      }
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

  public List<Value> vectors() {
    var collect = new ArrayList<Value>();
    if (languages.contains(Language.ENSO)) {
      collect.add(v(null, "", "[1,2,3]").type());
      collect.add(v(null, "", "[]").type());
      collect.add(v(null, "", "['a', 2, 0]").type());
      collect.add(v(null, "", "['a', 'b', 'c']").type());
      collect.add(v(null, "from Standard.Base.Nothing import Nothing", "[Nothing, Nothing]").type());
      collect.add(v(null, "from Standard.Base.Nothing import Nothing", "[Nothing, 'fff', 0, Nothing]").type());
    }
    return collect;
  }

  public List<Value> maps() {
    var collect = new ArrayList<Value>();
    if (languages.contains(Language.ENSO)) {
      var imports = """
          import Standard.Base.Data.Map.Map
          import Standard.Base.Nothing.Nothing
          """;
      for (var expr : List.of(
          "Map.empty",
          "Map.singleton Nothing Nothing",
          "Map.singleton Nothing 'my_value'",
          "Map.singleton 'my_value' Nothing",
          "Map.singleton 1 1",
          "Map.singleton 'C' 3",
          "Map.singleton 'C' 43",
          "Map.empty.insert 'A' 10 . insert 'B' 20",
          // ((int) 'A') + ((int) 'B') = 131 ; codePoint(131) = \203
          "Map.singleton '\203' 30",
          "Map.singleton Map.empty 1",
          "Map.singleton Map.empty Map.empty",
          "Map.empty.insert 1 1 . insert 2 2",
          "Map.empty.insert Nothing 'val' . insert 'key' 42",
          "Map.empty.insert 'A' 1 . insert 'B' 2 . insert 'C' 3",
          "Map.empty.insert 'C' 3 . insert 'B' 2 . insert 'A' 1"
      )) {
        collect.add(v("maps-" + expr, imports, expr, "Map").type());
      }
    }
    return collect;
  }

  public List<Value> multiLevelAtoms() {
    var collect = new ArrayList<Value>();
    if (languages.contains(Language.ENSO)) {
      var nodeTypeDef = """
          type Node
              C1 f1
              C2 f1 f2
              C3 f1 f2 f3
              Nil
              Value value
          """;
      var exprs = List.of(
          "Node.C2 Node.Nil (Node.Value 42)",
          "Node.C2 (Node.Value 42) Node.Nil",
          "Node.Nil",
          "Node.Value 42",
          "Node.Value 2",
          "Node.Value 2.0",
          "Node.C1 (Node.Value 42)",
          "Node.C1 Node.Nil",
          "Node.C3 Node.Nil (Node.Value 42) Node.Nil",
          "Node.C3 (Node.Value 42) Node.Nil Node.Nil",
          "Node.C3 Node.Nil Node.Nil Node.Nil",
          "Node.C2 (Node.C2 (Node.C1 Node.Nil) (Node.C1 (Node.C1 Node.Nil))) (Node.C2 (Node.C3 (Node.Nil) (Node.Value 22) (Node.Nil)) (Node.C2 (Node.Value 22) (Node.Nil)))",
          "Node.C2 (Node.C2 (Node.C1 Node.Nil) (Node.C1 Node.Nil)) (Node.C2 (Node.C3 (Node.Nil) (Node.Value 22) (Node.Nil)) (Node.C2 (Node.Value 22) (Node.Nil)))",
          "Node.C2 (Node.C2 (Node.C1 Node.Nil) (Node.C1 Node.Nil)) (Node.C2 (Node.C3 (Node.Nil) (Node.Nil) (Node.Value 22)) (Node.C2 (Node.Value 22) (Node.Nil)))"
      );
      collect.addAll(createValuesOfCustomType(nodeTypeDef, exprs, null));
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
      assertTrue("It can be executed " + v, v.canExecute());
    }
    return collect;
  }

  public Value typeSumType() {
    return constructorsAndValuesAndSumType().get(0);
  }

  public List<Value> constructorsAndValuesAndSumType() {
    var collect = new ArrayList<Value>();
    if (languages.contains(Language.ENSO)) {
      var code = """
        type Sum_Type
            Variant_A x
            Variant_B y
          """;
      var constructors = List.of(
          "Sum_Type",
          "Sum_Type.Variant_A",
          "Sum_Type.Variant_B",
          "Sum_Type.Variant_A 'A'",
          "Sum_Type.Variant_B 'B'"
      );
      var constructorTypes = Arrays.asList(new String[] {
          "Sum_Type",
          null,
          null,
          null,
          null
      });
      collect.addAll(createValuesOfCustomType(code, constructors, constructorTypes));
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
