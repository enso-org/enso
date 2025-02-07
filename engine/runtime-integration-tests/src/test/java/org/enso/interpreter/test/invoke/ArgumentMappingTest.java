package org.enso.interpreter.test.invoke;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import org.enso.interpreter.node.expression.constant.ConstantObjectNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.junit.Test;

/** Test suite for {@link org.enso.interpreter.runtime.callable.argument.CallArgumentInfo}. */
public final class ArgumentMappingTest {
  @Test
  public void oneArgument() {
    var funcSchema =
        FunctionSchema.newBuilder()
            .argumentDefinitions(
                new ArgumentDefinition(
                    0, "self", null, null, ArgumentDefinition.ExecutionMode.EXECUTE))
            .build();
    CallArgumentInfo[] callArgInfos = new CallArgumentInfo[] {new CallArgumentInfo("self")};
    var argMapping = CallArgumentInfo.ArgumentMappingBuilder.generate(funcSchema, callArgInfos);
    var postAppSchema = argMapping.getPostApplicationSchema();
    assertThat(
        "Post application schema should be fully applied",
        postAppSchema.isFullyApplied(),
        is(true));
    assertThat(
        "Pre-application schema and post-application schema should be the same",
        postAppSchema.getArgumentInfos()[0],
        is(funcSchema.getArgumentInfos()[0]));
  }

  @Test
  public void oneDefaultArgument_OnSecondPosition() {
    var funcSchema =
        FunctionSchema.newBuilder()
            .argumentDefinitions(
                new ArgumentDefinition(
                    0, "x", null, null, ArgumentDefinition.ExecutionMode.EXECUTE),
                new ArgumentDefinition(
                    1,
                    "y",
                    null,
                    ConstantObjectNode.build(42L),
                    ArgumentDefinition.ExecutionMode.EXECUTE))
            .build();
    CallArgumentInfo[] callArgInfos = new CallArgumentInfo[] {new CallArgumentInfo("x")};
    var argMapping = CallArgumentInfo.ArgumentMappingBuilder.generate(funcSchema, callArgInfos);
    var postAppSchema = argMapping.getPostApplicationSchema();
    assertThat(argMapping.getArgumentShouldExecute(), is(new boolean[] {true}));
    assertThat(postAppSchema.cloneHasPreApplied(), is(new boolean[] {true, false}));
  }

  @Test
  public void oneDefaultArgument_OnFirstPositionPosition() {
    var funcSchema =
        FunctionSchema.newBuilder()
            .argumentDefinitions(
                new ArgumentDefinition(
                    0,
                    "x",
                    null,
                    ConstantObjectNode.build(42L),
                    ArgumentDefinition.ExecutionMode.EXECUTE),
                new ArgumentDefinition(
                    1, "y", null, null, ArgumentDefinition.ExecutionMode.EXECUTE))
            .build();
    CallArgumentInfo[] callArgInfos = new CallArgumentInfo[] {new CallArgumentInfo("y")};
    var argMapping = CallArgumentInfo.ArgumentMappingBuilder.generate(funcSchema, callArgInfos);
    var postAppSchema = argMapping.getPostApplicationSchema();
    assertThat(argMapping, is(notNullValue()));
    assertThat(argMapping.getArgumentShouldExecute(), is(new boolean[] {true}));
    assertThat(postAppSchema.cloneHasPreApplied(), is(new boolean[] {false, true}));
  }
}
