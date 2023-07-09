package org.enso.interpreter.bench

object IRUtils {
  /*
  private def collectBenchCalls(ir: IR.Module): List[BenchCall] = {
    val methods = ir.bindings.collect { case m : IR.Module.Scope.Definition.Method => m }

    for (method <- methods) {
      method.body.preorder.flatMap(ir => {
        ir match {
          case application: IR.Application.Prefix =>
            application.function match {
              case IR.Name.Literal("measure", true, _, _, _) =>
                application.arguments.head match {
                  case IR.CallArgument.Specified(_, value, _, _, _) =>
                    value match {
                      case IR.Name.Literal("Bench", _, _, _, _) =>
                        Some(new BenchCall(application))
                      case _ => None
                    }
                }
              case _ => None
            }
        }
      })
    }
  }

  private class BenchCall(
    val call: IR.Application.Prefix
  ) {
    val args: List[IR.CallArgument.Specified] = call.arguments.collect { case s : IR.CallArgument.Specified => s }
    val location: IR.IdentifiedLocation = call.location.get
    assert(args.length == 5)
    assert(args.head.value.asInstanceOf[IR.Name.Literal].name == "Bench")
    val action: IR.Expression = args(1).value
    val title: String = {
      args(2).value match {
        case lit: IR.Literal.Text => lit.text
        case unexpected => throw new IllegalStateException(s"Unexpected bench title IR: ${unexpected}")
      }
    }
  }
  */
}
