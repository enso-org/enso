package org.enso.benchmarks.processor;

import java.util.ArrayList;
import java.util.List;
import org.enso.benchmarks.BenchGroup;
import org.enso.benchmarks.BenchSuite;
import org.enso.benchmarks.ModuleBenchSuite;
import org.enso.polyglot.MethodNames.Module;
import org.graalvm.polyglot.Value;

/**
 * Collect benchmark specifications from Enso source files.
 */
public class SpecCollector {
  private SpecCollector() {}

  /**
   * Collects all the bench specifications from the given module in a variable with the given name.
   * @param varName Name of the variable that holds all the collected bench suites.
   * @return Empty list if no such variable exists, or if it is not a vector.
   */
  public static List<ModuleBenchSuite> collectBenchSpecsFromModule(Value module, String varName) {
    Value moduleType = module.invokeMember(Module.GET_ASSOCIATED_TYPE);
    Value allSuitesVar = module.invokeMember(Module.GET_METHOD, moduleType, varName);
    String moduleQualifiedName = module.invokeMember(Module.GET_NAME).asString();
    if (!allSuitesVar.isNull()) {
      Value suitesValue = module.invokeMember(Module.EVAL_EXPRESSION, varName);
      if (!suitesValue.hasArrayElements()) {
        return List.of();
      }
      List<ModuleBenchSuite> suites = new ArrayList<>();
      for (long i = 0; i < suitesValue.getArraySize(); i++) {
        Value suite = suitesValue.getArrayElement(i);
        BenchSuite benchSuite = suite.as(BenchSuite.class);
        suites.add(
            new ModuleBenchSuite(benchSuite, moduleQualifiedName)
        );
      }
      return suites;
    }
    return List.of();
  }

  /**
   * Collects all the bench specifications from the given module in a variable with the given name.
   * @param groupName Name of the benchmark group
   * @param varName Name of the variable that holds all the collected bench suites.
   * @return null if no such group exists.
   */
  public static BenchGroup collectBenchGroupFromModule(Value module, String groupName, String varName) {
    var specs = collectBenchSpecsFromModule(module, varName);
    for (ModuleBenchSuite suite : specs) {
      BenchGroup group = suite.findGroupByName(groupName);
      if (group != null) {
        return group;
      }
    }
    return null;
  }
}
