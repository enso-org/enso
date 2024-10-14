package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.version.BuildVersion;

@BuiltinMethod(
    type = "Meta",
    name = "engine_version",
    description = "Returns the version of the currently running Enso engine.",
    autoRegister = false)
public class CurrentEngineVersionNode extends Node {

  public Text execute() {
    StringBuilder sb = new StringBuilder();
    sb.append("Enso Engine Version: ");
    sb.append(BuildVersion.ensoVersion());
    sb.append("\nDefault Edition: ");
    sb.append(BuildVersion.currentEdition());

    sb.append("\nCompiled with GraalVM ");
    sb.append(BuildVersion.graalVersion());
    sb.append(", Scalac ");
    sb.append(BuildVersion.scalacVersion());

    sb.append("\nBased on commit ");
    sb.append(BuildVersion.commit());
    sb.append(" (at ");
    sb.append(BuildVersion.latestCommitDate());
    sb.append(")\non ref ");
    sb.append(BuildVersion.ref());
    if (BuildVersion.isDirty()) {
      sb.append("\n(with uncommitted changes)");
    }

    return Text.create(sb.toString());
  }
}
