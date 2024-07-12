package org.enso.languageserver.boot

/** A wrapper around an existing component/resource that can be added with a delay and ensured
  * to be closed safely.
  */
class ComponentSupervisor extends AutoCloseable {
  private var component: AutoCloseable = null

  def registerService(component: AutoCloseable): Unit = {
    assert(this.component == null, "can't register component twice")
    this.component = component
  }

  override def close(): Unit = {
    if (this.component != null) {
      this.component.close()
      this.component = null
    }
  }
}
