import type { VueHost } from '@/components/VueHostRender.vue'
import { WidgetType } from '@codemirror/view'
import { h, markRaw, type Component } from 'vue'

/** Common base class for any Vue-based CodeMirror widget */
export class VueDecorationWidget<Props> extends WidgetType {
  private container: HTMLElement | undefined
  private vueHostRegistration: { unregister: () => void } | undefined
  protected readonly widget: Component
  protected readonly props: Props
  protected readonly vueHost: VueHost
  protected readonly className: string
  protected readonly elementType: string = 'div'

  /** Constructor. */
  constructor(
    widget: Component,
    props: Props,
    vueHost: VueHost,
    className: string,
    elementType: string = 'div',
  ) {
    super()
    this.widget = widget
    this.props = props
    this.vueHost = vueHost
    this.className = className
    this.elementType = elementType
  }

  /** See {@link WidgetType.estimatedHeight}. */
  override get estimatedHeight() {
    return -1
  }

  /** See {@link WidgetType.toDOM}. */
  override toDOM(): HTMLElement {
    if (!this.container) {
      const container = markRaw(document.createElement(this.elementType))
      container.className = this.className
      this.vueHostRegistration = this.vueHost.register(() => h(this.widget, this.props), container)
      this.container = container
    }
    return this.container
  }

  /** See {@link WidgetType.destroy}. */
  override destroy() {
    this.vueHostRegistration?.unregister()
    this.container = undefined
  }
}
