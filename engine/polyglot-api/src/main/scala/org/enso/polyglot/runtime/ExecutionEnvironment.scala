package org.enso.polyglot.runtime

import com.github.plokhotnyuk.jsoniter_scala.macros.named

/** Base trait for runtime execution environment. */
sealed trait ExecutionEnvironment {

  /** The environment name. */
  def name: String
}
object ExecutionEnvironment {

  @named("executionEnvironmentLive")
  final case class Live() extends ExecutionEnvironment {

    /** @inheritdoc */
    override val name: String = "live"
  }

  @named("executionEnvironmentDesign")
  final case class Design() extends ExecutionEnvironment {

    /** @inheritdoc */
    override val name: String = "design"
  }
}
