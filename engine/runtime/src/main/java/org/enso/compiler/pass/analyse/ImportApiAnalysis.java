package org.enso.compiler.pass.analyse;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.UUID;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.IR$Error$ImportExport;
import org.enso.compiler.core.IR$Error$ImportExport$SymbolDoesNotExist;
import org.enso.compiler.core.IR$Module$Scope$Import$Module;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.BindingsMap$ModuleReference$Concrete;
import org.enso.compiler.pass.IRPass;
import org.enso.compiler.pass.desugar.ComplexType$;
import org.enso.compiler.pass.desugar.FunctionBinding$;
import org.enso.compiler.pass.desugar.GenerateMethodBodies$;
import org.enso.compiler.pass.resolve.MethodDefinitions$;
import org.enso.compiler.pass.resolve.Patterns$;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.util.ScalaConversions;
import org.enso.pkg.QualifiedName;
import scala.collection.immutable.Seq;

/** Verifies that all imported symbols are also accessible via {@code Main.enso}.
 * Checks if a package has {@code Main.enso} file and if so it replaces all
 * imports that bypass that file with an error.
 */
public class ImportApiAnalysis implements IRPass {
  public static final ImportApiAnalysis MODULE$ = new ImportApiAnalysis();
  private UUID uuid;

  private ImportApiAnalysis() {
  }

  @Override
  public void org$enso$compiler$pass$IRPass$_setter_$key_$eq(UUID v) {
    this.uuid = v;
  }

  @Override
  public UUID key() {
    return uuid;
  }

  @Override
  public Seq<IRPass> precursorPasses() {
    return cons(ComplexType$.MODULE$, cons(FunctionBinding$.MODULE$, cons(GenerateMethodBodies$.MODULE$, nil())));
  }

  @Override
  public Seq<IRPass> invalidatedPasses() {
    return cons(MethodDefinitions$.MODULE$, cons(Patterns$.MODULE$, nil()));
  }

  @Override
  public IR.Module runModule(IR.Module ir, ModuleContext moduleContext) {
    var map = (BindingsMap) ir.passData().get(BindingAnalysis$.MODULE$).get();
    var forbiddenImports = new HashMap<String, String>();

    for (var imp : ScalaConversions.asJava(map.resolvedImports())) {
      var mod = switch (imp.target()) {
        case BindingsMap.ResolvedModule rm ->
          switch (rm.module()) {
            case BindingsMap$ModuleReference$Concrete c ->
              c.module();
            default ->
              null;
          };
        default ->
          null;
      };

      if (mod != null) {
        var pkg = mod.getPackage();
        if (moduleContext.module().getPackage() != pkg) {
          if (pkg.mainFile() != null) {
            if (pkg.findModule("Main") instanceof Module mainModule) {
              // if the package has Main file, then imports has to go thru the main mod
              boolean forbidden;
              if (mod == mainModule) {
                forbidden = false;
              } else {
                forbidden = !findLogicalImport(mod, mainModule);
              }
              if (forbidden) {
                forbiddenImports.put(
                        mod.getName().toString(), mainModule.getName().toString()
                );
              }
            }
          }
        }
      }
    }

    var replace = ir.imports().map((imp) -> switch (imp) {
      case IR$Module$Scope$Import$Module mod -> {
        var name = mod.name().name();
        var pkg = forbiddenImports.get(name);
        if (pkg != null) {
          var segments = name.split("\\.");
          var last = segments[segments.length - 1];
          yield new IR$Error$ImportExport(
            imp,
            new IR$Error$ImportExport$SymbolDoesNotExist(last, pkg),
            imp.passData(), imp.diagnostics()
          );
        } else {
          yield mod;
        }
      }
      default ->
        imp;
    });
    return ir.copy(
            replace,
            ir.copy$default$2(),
            ir.copy$default$3(),
            ir.copy$default$4(),
            ir.copy$default$5(),
            ir.copy$default$6(),
            ir.copy$default$7()
    );
  }

  @Override
  public IR.Expression runExpression(IR.Expression ir, InlineContext inlineContext) {
    return ir;
  }

  @Override
  public <T extends IR> T updateMetadataInDuplicate(T sourceIr, T copyOfIr) {
    return copyOfIr;
  }

  /** @return {@code true} if the {@code mod} isn't allowed to be imported */
  private static boolean findLogicalImport(Module mod, Module rootModule) {
    var checked = new HashSet<>();
    var toProcess = new LinkedList<Module>();
    toProcess.add(rootModule);

    for (;;) {
      if (toProcess.isEmpty()) {
        break;
      }
      var current = toProcess.remove();
      if (!checked.add(current)) {
        continue;
      }
      var ir = current.getIr();
      if (ir == null) {
        var ctx = EnsoContext.get(null);
        ir = current.compileScope(ctx).getModule().getIr();
      }
      var meta = ir.passData();
      var map = (BindingsMap) meta.get(BindingAnalysis$.MODULE$).get();

      var itMainMap = map.exportedSymbols().iterator();
      while (itMainMap.hasNext()) {
        for (var module : ScalaConversions.asJava(itMainMap.next()._2())) {
          switch (module) {
            case BindingsMap.ResolvedModule rm -> {
              switch (rm.module()) {
                case BindingsMap$ModuleReference$Concrete allowed -> {
                  if (allowed.module() == mod) {
                    // found import
                    return true;
                  } else {
                    if (isParentModule(allowed.getName(), mod.getName())) {
                      toProcess.add(allowed.module());
                    }
                  }
                }
                default -> {
                }
              }
            }
            default -> {
            }
          }
        }
      }
    }
    // no logical import found
    return false;
  }

  private static boolean isParentModule(QualifiedName parent, QualifiedName mod) {
    var at = 0;
    var parentPath = parent.pathAsJava();
    var modPath = mod.pathAsJava();
    while (at < parentPath.size()) {
      if (at >= modPath.size()) {
        return false;
      }
      if (!modPath.get(at).equals(parentPath.get(at))) {
        return false;
      }
      at++;
    }
    return modPath.size() > at && parent.item().equals(modPath.get(at));
  }

  @SuppressWarnings("unchecked")
  private static <T> scala.collection.immutable.List<T> nil() {
    return (scala.collection.immutable.List<T>) scala.collection.immutable.Nil$.MODULE$;
  }
  private static <T> scala.collection.immutable.List<T> cons(T head, scala.collection.immutable.List<T> tail) {
    return scala.collection.immutable.$colon$colon$.MODULE$.apply(head, tail);
  }
}
