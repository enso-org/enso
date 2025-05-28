package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.warning.Warning;
import org.enso.interpreter.runtime.warning.WarningsLibrary;
import org.enso.interpreter.runtime.warning.WithWarnings;
import org.enso.test.utils.ContextUtils;
import org.junit.ClassRule;
import org.junit.Test;

public class ArrayTest {
  @ClassRule public static final ContextUtils ctx = ContextUtils.createDefault();

  @Test
  public void buildVectorOfLongAndOneWarning() throws Exception {
    var warn = Warning.create(ctx.ensoContext(), Text.create("Problematic"), 0);
    var ww = WithWarnings.create(17L, 1, false, warn);
    var arr = ArrayLikeHelpers.wrapObjectsWithCheckAt(42, ww, -34);
    var warnLib = WarningsLibrary.getUncached();

    {
      // check one warning in arr
      assertTrue("Has warnings", warnLib.hasWarnings(arr));
      assertEquals("Array", arr.getClass().getSimpleName());
      var warnMap = ctx.context().asValue(warnLib.getWarnings(arr, false));
      assertEquals("One warning: " + warnMap, 1, warnMap.getHashSize());
      var firstWarn = warnMap.getHashValuesIterator().getIteratorNextElement();
      assertEquals("Our warning is present", warn, ctx.unwrapValue(firstWarn));
    }

    var cleanArr = warnLib.removeWarnings(arr);
    {
      assertFalse("No warning", warnLib.hasWarnings(cleanArr));
      var warnMap = ctx.context().asValue(warnLib.getWarnings(cleanArr, false));
      assertEquals("No map of warnings", 0, warnMap.getHashSize());
    }
  }
}
