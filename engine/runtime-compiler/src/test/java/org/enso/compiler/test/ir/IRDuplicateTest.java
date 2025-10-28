package org.enso.compiler.test.ir;

import static org.enso.scala.wrapper.ScalaConversions.asScala;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.List;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.Name.Literal;
import org.enso.compiler.core.ir.Pattern;
import org.enso.compiler.core.ir.expression.Case;
import org.junit.Rule;
import org.junit.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.quality.Strictness;
import scala.Option;

@ExtendWith(MockitoExtension.class)
public class IRDuplicateTest {
  @Rule public MockitoRule mockitoRule = MockitoJUnit.rule().strictness(Strictness.STRICT_STUBS);

  @Test
  public void duplicate_IR_WithoutChildren() {
    var passData = mock(MetadataStorage.class);
    var passDataDuplicate = mock(MetadataStorage.class);
    when(passData.duplicate()).thenReturn(passDataDuplicate);
    lenient()
        .when(passData.copy())
        .thenThrow(new AssertionError("copy() should not be called on passData"));

    var lit = new Name.Literal("x", false, null, Option.empty(), passData);
    var duplicate = lit.duplicate(true, true, true, true);
    assertThat(duplicate.passData(), is(passDataDuplicate));
    assertThat("Old passData stays the same", lit.passData(), is(passData));
  }

  @Test
  public void duplicate_QualifiedName_WithChildren() {
    var passData = mock(MetadataStorage.class);
    var passDataDuplicate = mock(MetadataStorage.class);
    when(passData.duplicate()).thenReturn(passDataDuplicate);
    lenient()
        .when(passData.copy())
        .thenThrow(new AssertionError("copy() should not be called on passData"));

    var lit = new Literal("x", false, null, Option.empty(), passData);
    var qualifiedName = new Name.Qualified(asScala(List.of(lit)), null, new MetadataStorage());

    var duplicate = qualifiedName.duplicate(true, true, true, true);
    assertThat(
        "Child's passData was duplicated",
        duplicate.parts().head().passData(),
        is(passDataDuplicate));
  }

  @Test
  public void duplicate_CaseExpr_WithChildren() {
    var patPassData = mock(MetadataStorage.class);
    var patPassDataDuplicate = mock(MetadataStorage.class);
    when(patPassData.duplicate()).thenReturn(patPassDataDuplicate);
    var pat =
        new Pattern.Literal(
            new org.enso.compiler.core.ir.Literal.Text("foo", null, new MetadataStorage()),
            null,
            patPassData);
    var branch =
        Case.Branch.builder()
            .terminalBranch(true)
            .expression(IRUtils.emptyIr())
            .pattern(pat)
            .build();
    var caseExpr =
        Case.Expr.builder()
            .scrutinee(IRUtils.emptyIr())
            .branches(asScala(List.of(branch)))
            .isNested(false)
            .build();
    var duplicate = caseExpr.duplicate(true, true, true, true);
    assertThat(
        "Child's passData was duplicated",
        duplicate.branches().head().pattern().passData(),
        is(patPassDataDuplicate));
  }
}
