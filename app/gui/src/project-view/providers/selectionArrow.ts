import type SelectionArrow from '@/components/GraphEditor/widgets/WidgetSelection/SelectionArrow.vue'
import { createContextStore } from '@/providers'
import type { PortId } from '@/providers/portInfo'
import { Ast } from '@/util/ast'
import { proxyRefs, type ToValue } from '@/util/reactivity'
import { identity } from '@vueuse/core'
import {
  computed,
  ref,
  toValue,
  watch,
  type ComputedRef,
  type RendererElement,
  type RendererNode,
} from 'vue'
import type { ComponentProps } from 'vue-component-type-helpers'

interface SelectionArrowInfo {
  /** Id of the subexpression that should display arrow underneath. */
  id: Ast.AstId | PortId | Ast.TokenId | null
  /** Child widget can call this callback to request teleport of the arrow to specified element. */
  requestArrow: (to: RendererElement) => void
  /**
   * Whether or not the arrow provided by this context instance was already requested.
   * Do not request the arrow twice, it will be stolen from other elements!
   */
  handled: boolean
  /**
   * Child widget may set this flag to suppress arrow displaying.
   *
   * A usage example is a child suppressing arrow on hover, because interactions with this child
   * will not open the drop-down (because the child is drop-down itself, for examle).
   */
  suppressArrow: boolean
}

const [provideSelectionArrowInfo, injectSelectionArrow] = createContextStore(
  'Selection arrow info',
  identity<SelectionArrowInfo>,
)
export { injectSelectionArrow }

function selectionArrowTarget(ast: Ast.Expression): Ast.Expression | Ast.Token | null {
  let node = ast
  // If the input is a constructor application, place the arrow under the constructor name.
  while (node instanceof Ast.Ast) {
    if (node instanceof Ast.AutoscopedIdentifier) return node.identifier
    else if (node instanceof Ast.PropertyAccess) return node.rhs
    else if (node instanceof Ast.App) node = node.function
    else if (node instanceof Ast.Group && node.expression) node = node.expression
    else break
  }
  return null
}

interface SelectionArrowOptions {
  node: ToValue<Ast.Expression | unknown>
  show: ToValue<boolean>
  isHovered: ToValue<boolean>
}
/**
 * Creates a context store for a selection arrow location for a widget, and returns the
 * {@link SelectionArrow} properties to render it.
 */
export function provideSelectionArrow({
  isHovered,
  show,
  node,
}: SelectionArrowOptions): ComputedRef<ComponentProps<typeof SelectionArrow> | null> {
  const parentSelectionArrow = injectSelectionArrow(true)
  const arrowSuppressed = ref(false)
  const showArrow = computed(() => !arrowSuppressed.value && (toValue(show) || toValue(isHovered)))
  const arrowLocation = ref()
  provideSelectionArrowInfo(
    proxyRefs({
      id: computed((): Ast.AstId | Ast.TokenId | null => {
        const ast = toValue(node)
        if (!(ast instanceof Ast.Ast)) return null
        if (!ast.isExpression()) return null
        const target = selectionArrowTarget(ast)
        return target ? target.id : null
      }),
      requestArrow: (target: RendererNode) => {
        arrowLocation.value = target
      },
      handled: false,
      get suppressArrow() {
        return arrowSuppressed.value
      },
      set suppressArrow(value) {
        arrowSuppressed.value = value
      },
    }),
  )

  watch(showArrow, (arrowShown) => {
    if (parentSelectionArrow) parentSelectionArrow.suppressArrow = arrowShown
  })

  const arrow = computed(() => ({ location: arrowLocation.value, isHovered: toValue(isHovered) }))

  return computed(() => (showArrow.value ? arrow.value : null))
}
