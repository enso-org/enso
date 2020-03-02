package org.enso.compiler.pass

/**
  * A pass is a transformation from source type to sink type.
  *
  * Passes may take in additional information when run (e.g. analysis output),
  * and may also output additional information.
  *
  * @tparam In the source type
  * @tparam Out the sink type
  */
trait Pass[In, Out] {

  /**
    * A class representing the output of a pass.
    *
    * @param result the result of running the pass
    * @param metadata any metadata produced by the pass
    * @tparam TOut the type of the pass output metadata
    */
  sealed case class Output[TOut](result: Out, metadata: TOut)

  /**
    * Executes the pass on the source, with optional input metadata.
    *
    * @param input the source to transform or analyse
    * @param data metadata necessary foe the pass to execute correctly
    * @tparam TIn the type of the input metadata
    * @tparam TOut the type of the output metadata
    * @return the results of executing the pass on `input` and `data`, as well
    *         as any metadata the pass produces
    */
  def run[TIn, TOut](input: In, data: TIn): Output[TOut]
}
