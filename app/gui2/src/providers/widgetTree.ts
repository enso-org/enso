import type { Rect } from '@/util/rect'
import { ObservableV2 } from 'lib0/observable'
import type { ExprId } from 'shared/yjsModel'
import { type Ref } from 'vue'
import { createContextStore } from '.'

export { injectFn as injectWidgetTree, provideFn as provideWidgetTree }
const { provideFn, injectFn } = createContextStore(
  'Widget tree',
  (nodeSpanStart: Ref<number>) => new WidgetTree(nodeSpanStart),
)

type TreeEvent = {
  rect(expr: ExprId, rect: Rect): void
}

export class WidgetTree extends ObservableV2<TreeEvent> {
  constructor(public nodeSpanStart: Ref<number>) {
    super()
  }

  updateRect(expr: ExprId, rect: Rect) {
    this.emit('rect', [expr, rect])
  }
}
