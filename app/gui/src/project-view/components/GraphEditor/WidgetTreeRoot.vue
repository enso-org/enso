<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { useTransitioning } from '@/composables/animation'
import { useLayoutAnimationsState } from '@/providers/animationCounter'
import { WidgetInput, type WidgetUpdate } from '@/providers/widgetRegistry'
import { WidgetEditHandlerParent } from '@/providers/widgetRegistry/editHandler'
import { provideWidgetTree } from '@/providers/widgetTree'
import { Ast } from '@/util/ast'
import { computed, toRef, watch } from 'vue'
import { AstId } from 'ydoc-shared/ast'
import { ExternalId } from 'ydoc-shared/yjsModel'

const props = defineProps<{
  externalId: string & ExternalId
  input: WidgetInput
  rootElement: HTMLElement | undefined
  potentialSelfArgumentId?: AstId | undefined
  /** Ports that are not targetable by default; see {@link NodeDataFromAst}. */
  conditionalPorts?: Set<Ast.AstId> | undefined
  extended: boolean
  onUpdate: (update: WidgetUpdate) => boolean
}>()
const emit = defineEmits<{
  currentEditChanged: [WidgetEditHandlerParent | undefined]
}>()

const layoutTransitions = useTransitioning(
  new Set([
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
  ]),
)
const layoutAnimations = useLayoutAnimationsState()

const anyLayoutAnimationActive = computed(
  () => layoutTransitions.active.value || layoutAnimations.anyAnimationActive,
)

const tree = provideWidgetTree(
  toRef(props, 'externalId'),
  toRef(props, 'rootElement'),
  toRef(props, 'conditionalPorts'),
  toRef(props, 'extended'),
  anyLayoutAnimationActive,
  toRef(props, 'potentialSelfArgumentId'),
)
watch(toRef(tree, 'currentEdit'), (edit) => emit('currentEditChanged', edit))
</script>
<script lang="ts">
export const GRAB_HANDLE_X_MARGIN_L = 4
export const GRAB_HANDLE_X_MARGIN_R = 8
export const ICON_WIDTH = 16
</script>

<template>
  <div class="WidgetTreeRoot widgetRounded" spellcheck="false" v-on="layoutTransitions.events">
    <NodeWidget :input="input" :onUpdate="onUpdate" />
  </div>
</template>

<style scoped>
.WidgetTreeRoot {
  color: var(--color-node-text);

  outline: none;
  min-height: var(--node-port-height);
  display: flex;
  align-items: center;
}

/**
 * Implementation of token padding and its propagation through the widget tree.
 *
 * In widget tree, the margins around widgets require special care due to unusual set of
 * desing requirements. When a node or a port contains a widget that "fits nicely" with
 * within rounded corners, there shouldn't be any added margin between them. On the other
 * hand, when a widget ends with a text node without any rounded container, it needs to
 * maintain a certain padding from parent's rounding (e.g. rounding of the node shape).
 *
 * To implement that, we need to propagate the information of required left/right padding
 * throughout the tree structure, and allow widgets to either modify their requirements, or
 * to apply the required padding. We are using a set of special tree-scoped classes for that:
 *
 * - `.widgetRounded`: Signals that this widget has rounding, so it expects its content to
 *                     have added padding when appropriate. All widgets that have 24px rounded
 *                     corners need to have this class (e.g. ports, value inputs).
 *
 * - `.widgetResetRounding`: Resets the "rounding" state, setting padding expectation to 0.
 *                           This allows the widget to implement its own padding that will be
 *                           kept constant no matter the situation (e.g. `TopLevelArgument`).
 *
 * - `.widgetApplyPadding`: Keep distance from rounded corners, apply padding as required by
 *                          parent widget structure. This should be applied to *all* text-only
 *                          elements of a widget, anything that is or looks like a token.
 *
 * - `.widgetOutOfLayout`: An element that exists within a widget tree, but isn't taking any
 *                         visible horizontal space. Those elements are ignored when propagating
 *                         the padding information. It is important to add this class to any DOM
 *                         element with absolute positioning when it is placed at the beginning or
 *                         at the end of the widget template, so it doesn't prevent tokens around
 *                         them from being properly padded.
 */
.WidgetTreeRoot {
  /*
   * Core of the propagation logic. Prevent left/right margin from propagating to non-first non-last
   * children of a widget. That way, only the innermost left/right deep child of a rounded widget will
   * receive the propagated paddings.
   */
  *:not(:nth-child(1 of :not(.widgetOutOfLayout, [data-transitioning='leave']))) {
    --widget-token-pad-left: 0px;
  }
  *:not(:nth-last-child(1 of :not(.widgetOutOfLayout, [data-transitioning='leave']))) {
    --widget-token-pad-right: 0px;
  }

  /*
   * Any rounded widget sets expected padding variable, which is automatically inherited
   * by all its children.
   * Note that since the node itself is rounded, it behaves as a rounded container.
   */
  &,
  :deep(.widgetRounded.widgetRounded) {
    --widget-token-pad-left: var(--widget-token-pad-unit);
    --widget-token-pad-right: var(--widget-token-pad-unit);
  }

  :deep(.widgetResetRounding.widgetResetPadding) {
    --widget-token-pad-left: 0px;
    --widget-token-pad-right: 0px;
  }

  :deep(.widgetApplyPadding.widgetApplyPadding) {
    margin-left: var(--widget-token-pad-left, 0);
    margin-right: var(--widget-token-pad-right, 0);
    transition: margin 0.2s ease-out;
  }
}
</style>
