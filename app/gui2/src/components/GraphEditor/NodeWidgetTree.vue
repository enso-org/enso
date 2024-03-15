<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { useTransitioning } from '@/composables/animation'
import { WidgetInput, type WidgetUpdate } from '@/providers/widgetRegistry'
import { provideWidgetTree } from '@/providers/widgetTree'
import { useGraphStore, type NodeId } from '@/stores/graph'
import { Ast } from '@/util/ast'
import type { Icon } from '@/util/iconName'
import { computed, ref, toRef } from 'vue'

const props = defineProps<{
  ast: Ast.Ast
  nodeId: NodeId
  icon: Icon
  connectedSelfArgumentId: Ast.AstId | undefined
  potentialSelfArgumentId: Ast.AstId | undefined
  extended: boolean
}>()
const emit = defineEmits<{
  openFullMenu: []
}>()
const graph = useGraphStore()
const rootPort = computed(() => {
  const input = WidgetInput.FromAst(props.ast)
  if (props.ast instanceof Ast.Ident && !graph.db.isKnownFunctionCall(props.ast.id)) {
    input.forcePort = true
  }
  return input
})

const observedLayoutTransitions = new Set([
  'margin-left',
  'margin-right',
  'margin-top',
  'margin-bottom',
  'padding-left',
  'padding-right',
  'padding-top',
  'padding-bottom',
  'width',
  'height',
])

function handleWidgetUpdates(update: WidgetUpdate) {
  const edit = update.edit ?? graph.startEdit()
  if (update.portUpdate) {
    const { value, origin } = update.portUpdate
    if (Ast.isAstId(origin)) {
      const ast =
        value instanceof Ast.Ast ? value
        : value == null ? Ast.Wildcard.new(edit)
        : undefined
      if (ast) {
        edit.replaceValue(origin as Ast.AstId, ast)
      } else if (typeof value === 'string') {
        edit.tryGet(origin)?.syncToCode(value)
      }
    } else {
      console.error(`[UPDATE ${origin}] Invalid top-level origin. Expected expression ID.`)
    }
  }
  graph.commitEdit(edit)
  // This handler is guaranteed to be the last handler in the chain.
  return true
}

/**
 * We have two goals for our DOM/CSS that are somewhat in conflict:
 * - We position widget dialogs drawn outside the widget, like dropdowns, relative to their parents. If we teleported
 *   them, we'd have to maintain their positions through JS; the focus hierarchy would also be affected.
 * - We animate showing/hiding conditionally-visible placeholder arguments; the implementation of this animation
 *   requires the use of `overflow-x: clip` for the placeholder argument. There doesn't seem to be any good alternative
 *   to clipping in order to achieve a suitable style of animation with CSS.
 * Because clipping is absolute (there is no way for an element to draw outside its clipped ancestor), it is hard to
 * reconcile with dropdowns.
 *
 * However, we can have our cake and eat it to--as long as we don't need both at once. The solution implemented here is
 * for the widget tree to provide an interface for a widget to signal that it is in a state requiring drawing outside
 * the node, and for widgets implementing clipping-based animations to mark them with a CSS class.
 *
 * This is not a perfect solution; it's possible for the user to cause a dropdown to be displayed before the
 * showing-placeholders animation finishes. In that case the animation will run without clipping, which looks a little
 * off. However, it allows us to use the DOM/CSS both for positioning the dropdown and animating the placeholders.
 */
const deepDisableClipping = ref(false)

const layoutTransitions = useTransitioning(observedLayoutTransitions)
provideWidgetTree(
  toRef(props, 'ast'),
  toRef(props, 'nodeId'),
  toRef(props, 'icon'),
  toRef(props, 'connectedSelfArgumentId'),
  toRef(props, 'potentialSelfArgumentId'),
  toRef(props, 'extended'),
  layoutTransitions.active,
  () => {
    emit('openFullMenu')
  },
  (clippingInhibitorsExist) => (deepDisableClipping.value = clippingInhibitorsExist),
)
</script>
<script lang="ts">
export const GRAB_HANDLE_X_MARGIN = 4
const GRAB_HANDLE_X_MARGIN_PX = `${GRAB_HANDLE_X_MARGIN}px`
export const ICON_WIDTH = 16
</script>

<template>
  <div
    class="NodeWidgetTree"
    :class="{ deepDisableClipping }"
    spellcheck="false"
    v-on="layoutTransitions.events"
  >
    <!-- Display an icon for the node if no widget in the tree provides one. -->
    <SvgIcon
      v-if="!props.connectedSelfArgumentId"
      class="icon grab-handle nodeCategoryIcon"
      :name="props.icon"
      @click.right.stop.prevent="emit('openFullMenu')"
    />
    <NodeWidget :input="rootPort" @update="handleWidgetUpdates" />
  </div>
</template>

<style scoped>
.NodeWidgetTree {
  color: white;

  outline: none;
  height: 24px;
  display: flex;
  align-items: center;

  &:has(.WidgetPort.newToConnect) {
    margin-left: calc(4px - var(--widget-port-extra-pad));
  }

  &:has(.WidgetPort.newToConnect > .r-24:only-child) {
    margin-left: 0px;
  }
}

.GraphEditor.draggingEdge .NodeWidgetTree {
  transition: margin 0.2s ease;
}

.icon {
  margin-right: 4px;
}

.grab-handle {
  color: white;
  margin: 0 v-bind('GRAB_HANDLE_X_MARGIN_PX');
}

.deepDisableClipping :deep(.overridableClipState) {
  overflow: visible !important;
}
</style>
