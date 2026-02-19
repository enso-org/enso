package org.enso.table.data.column.operation.unary;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import org.enso.table.data.column.operation.cast.ConversionFailure;
import org.enso.table.data.column.operation.cast.ConversionFailureType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.test.utils.ContextUtils;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

public class CastOperationTest {
  @ClassRule public static final ContextUtils ctx = ContextUtils.createDefault();

  @BeforeClass
  public static void importTable() {
    var m =
        ctx.evalModule(
            """
            from Standard.Table import all

            main = Table
            """);
    assertFalse("Make sure Standard.Table is imported", m.isNull());
  }

  @Test
  public void caseOfOnConversionFailureRelatedColumn() {
    var cf =
        new ConversionFailure(
            ConversionFailureType.FAILED_CONVERSION, TextType.VARIABLE_LENGTH, null, 0, null);
    var v = cf.asEnsoValue();

    var matchNull =
        ctx.evalModule(
            """
            from Standard.Base import all
            match_null v = case v.related_column of
              Nothing -> "Good"
              _ -> "Bad"
            """,
            "match_null");

    var res = matchNull.execute(v);
    assertEquals("Good", res.asString());
  }
}
