import type { VueHost } from '@/components/VueHostRender.vue'
import type { TooltipView } from '@codemirror/view'
import { h, markRaw, type Component } from 'vue'

/** Creates a {@link TooltipView} for a Vue component. */
export function vueTooltipView<Props>(
  vueHost: VueHost,
  widget: Component,
  props: Props,
): TooltipView {
  const container = markRaw(document.createElement('div'))
  const { unregister } = vueHost.register(h(widget, props), container)
  return { dom: container, destroy: unregister, resize: false }
}
