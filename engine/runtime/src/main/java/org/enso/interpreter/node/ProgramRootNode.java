package org.enso.interpreter.node;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import java.io.File;
import java.util.LinkedList;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.Module;
import org.enso.pkg.Package;
import org.enso.pkg.QualifiedName;
import org.enso.scala.wrapper.ScalaConversions;
import org.slf4j.LoggerFactory;

/**
 * This node handles static transformation of the input AST before execution and represents the root
 * of an Enso program.
 *
 * <p>As much of the static transformation and analysis functionality required by the interpreter
 * must have access to the interpreter, it must take place as part of the interpreter context. As a
 * result, this node handles the transformations and re-writes
 */
@NodeInfo(shortName = "ProgramRoot", description = "The root of an Enso program's execution")
public class ProgramRootNode extends RootNode {
  private final Source sourceCode;
  private @CompilerDirectives.CompilationFinal Module module;

  ProgramRootNode(EnsoLanguage language, Source sourceCode) {
    super(language);
    this.sourceCode = sourceCode;
  }

  @Override
  @CompilerDirectives.TruffleBoundary
  public String getName() {
    var segs = sourceCode.getName().split("\\.");
    if (segs.length == 0) {
      return "Unnamed";
    } else {
      return segs[0];
    }
  }

  @Override
  @CompilerDirectives.TruffleBoundary
  public SourceSection getSourceSection() {
    return sourceCode.createSection(0, sourceCode.getLength());
  }

  /**
   * Constructs the root node.
   *
   * @param language the language instance in which this will execute
   * @param sourceCode the code to compile and execute
   * @return a program root node
   */
  public static ProgramRootNode build(EnsoLanguage language, Source sourceCode) {
    return new ProgramRootNode(language, sourceCode);
  }

  /**
   * Executes the static analysis passes before executing the resultant program.
   *
   * @param frame the stack frame to execute in
   * @return the result of executing this node
   */
  @Override
  public Object execute(VirtualFrame frame) {
    if (module == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      var ctx = EnsoContext.get(this);
      if (sourceCode.getPath() != null) {
        var src = ctx.getTruffleFile(new File(sourceCode.getPath()));
        var pkg = ctx.getPackageOf(src).orElse(null);
        var qualifiedName = findQualifiedNameInPackage(pkg, src, getName());
        module = new Module(qualifiedName, pkg, src);
      } else {
        var simpleName = QualifiedName.simpleName(getName());
        module = new Module(simpleName, null, sourceCode.getCharacters().toString());
      }
      ctx.getPackageRepository().registerModuleCreatedInRuntime(module.asCompilerModule());
      if (ctx.isStrictErrors()) {
        module.compileScope(ctx);
      }
    }
    // Note [Static Passes]
    return module;
  }

  private static QualifiedName findQualifiedNameInPackage(
      Package<TruffleFile> pkg, TruffleFile src, String srcName) {
    if (pkg != null) {
      try {
        var rel = pkg.sourceDir().relativize(src.getParent());
        var names = new LinkedList<String>();
        while (rel != null) {
          if (!rel.getName().isEmpty()) {
            names.add(0, rel.getName());
          }
          rel = rel.getParent();
        }
        names.add(0, pkg.name());
        names.add(0, pkg.namespace());
        return QualifiedName.apply(ScalaConversions.asScala(names), srcName);
      } catch (IllegalStateException ex) {
        LoggerFactory.getLogger(ProgramRootNode.class)
            .warn("Cannot find package name for " + src, ex);
      }
    }
    return QualifiedName.simpleName(srcName);
  }

  /* Note [Static Passes]
   * ~~~~~~~~~~~~~~~~~~~~
   * Almost all of the static analysis functionality required by the interpreter requires access to
   * the interpreter to execute small amounts of code. This is for purposes such as:
   * - Type-level computation and evaluation during typechecking.
   * - Compile-Time Function Evaluation (CTFE) for optimisation.
   * - Various other re-write mechanisms that involve code execution.
   *
   * The contract expected from a Truffle Language states that there is to be no access to the
   * interpreter context during parsing, which is the most natural time to perform these
   * transformation passes. As a result, we have to perform them inside the interpreter once parsing
   * is completed.
   *
   * To that end, we have a special kind of root node. It is constructed with the input AST only,
   * and when executed it takes the input source and executes a sequence of analyses and
   * transformations such that the end result is a registration of all defined symbols in the
   * Language Context.
   *
   */

}
