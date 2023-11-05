import type { AstExtended } from '@/util/ast'
import type { Rect } from '@/util/rect'
import { ObservableV2 } from 'lib0/observable'
import type { ExprId } from 'shared/yjsModel'
import { reactive, type Ref } from 'vue'
import { createContextStore } from '.'

export { injectFn as injectWidgetTree, provideFn as provideWidgetTree }
const { provideFn, injectFn } = createContextStore(
  'Widget tree',
  (nodeSpanStart: Ref<number>) => new WidgetTree(nodeSpanStart),
)

interface WidgetSubtree {
  ast: AstExtended
  anchor: Element
}

type TreeEvent = {
  rect(expr: ExprId, rect: Rect): void
}

export class WidgetTree extends ObservableV2<TreeEvent> {
  // subtrees: Map<Element, WidgetSubtree> = reactive(new Map())
  constructor(public nodeSpanStart: Ref<number>) {
    super()
  }

  // setSubtree(ast: AstExtended, anchor: Element): () => void {
  //   this.subtrees.set(anchor, { ast, anchor })
  //   return () => {
  //     this.subtrees.delete(anchor)
  //   }
  // }
  // iterSubtrees(): IterableIterator<[string, WidgetSubtree]> {
  //   const usedIds = new Set<string>()
  //   return mapIterator(this.subtrees.values(), (subtree) => {
  //     const baseId = subtree.ast.astId
  //     let id: string = baseId
  //     let i = 0
  //     while (usedIds.has(id)) {
  //       id = `${baseId}:${i++}`
  //     }
  //     return [id, subtree]
  //   })
  // }
  updateRect(expr: ExprId, rect: Rect) {
    this.emit('rect', [expr, rect])
  }
}
