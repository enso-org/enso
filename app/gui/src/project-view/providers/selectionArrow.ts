import type SelectionArrow from '@/components/GraphEditor/widgets/WidgetSelection/SelectionArrow.vue'
import { createContextStore } from '@/providers'
import { Ast } from '@/util/ast'
import { proxyRefs, type ToValue } from '@/util/reactivity'
import { computed, ref, toValue, watch, type ComputedRef, type Ref, type RendererNode } from 'vue'
import type { ComponentProps } from 'vue-component-type-helpers'

const [provideSelectionArrowInfo, injectSelectionArrow] = createContextStore(
  'Selection arrow info',
  ({
    node,
    arrowLocation,
  }: {
    node: ToValue<Ast.Expression | unknown>
    arrowLocation: Ref<RendererNode | null>
  }) =>
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
      handled: ref(false),
      suppressArrow: ref(false),
    }),
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
  const arrowLocation = ref()
  const arrow = provideSelectionArrowInfo({ node, arrowLocation })

  const showArrow = computed(() => !arrow.suppressArrow && (toValue(show) || toValue(isHovered)))
  watch(showArrow, (arrowShown) => {
    if (parentSelectionArrow) parentSelectionArrow.suppressArrow = arrowShown
  })

  const arrowProps = computed(() => ({
    location: arrowLocation.value,
    isHovered: toValue(isHovered),
  }))

  return computed(() => (showArrow.value ? arrowProps.value : null))
}
