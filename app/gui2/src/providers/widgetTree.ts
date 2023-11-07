import { type Ref } from 'vue'
import { createContextStore } from '.'

export { injectFn as injectWidgetTree, provideFn as provideWidgetTree }
const { provideFn, injectFn } = createContextStore(
  'Widget tree',
  (nodeSpanStart: Ref<number>) => new WidgetTree(nodeSpanStart),
)

export class WidgetTree {
  constructor(public nodeSpanStart: Ref<number>) {}
}
