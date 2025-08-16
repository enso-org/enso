<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { useTransitioning } from '@/composables/animation'
import { useLayoutAnimationsState } from '@/providers/animationCounter'
import { UpdateHandler, WidgetInput } from '@/providers/widgetRegistry'
import { WidgetEditHandlerParent } from '@/providers/widgetRegistry/editHandler'
import { provideWidgetTree } from '@/providers/widgetTree'
import { emptyPrimaryApplication, type PrimaryApplication } from '@/stores/graph/graphDatabase'
import { Ast } from '@/util/ast'
import { Opt } from '@/util/data/opt'
import { templateRef } from '@vueuse/core'
import { computed, toRef, watch } from 'vue'
import { ExternalId } from 'ydoc-shared/yjsModel'
import WidgetTreeRootStyles from './WidgetTreeRootStyles.vue'

const props = defineProps<{
  externalId: string & ExternalId
  input: WidgetInput
  rootElement?: Opt<HTMLElement>
  primaryApplication?: Opt<PrimaryApplication>
  /** Ports that are not targetable by default; see {@link NodeDataFromAst}. */
  conditionalPorts?: Set<Ast.AstId> | undefined
  extended?: boolean
  updateCallback: UpdateHandler
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

const treeRoot = templateRef('treeRoot')
const rootElementWithFallback = computed(() => props.rootElement ?? treeRoot.value)

const primaryApplication = computed(() => props.primaryApplication ?? emptyPrimaryApplication())
const extended = computed(() => props.extended ?? false)
const tree = provideWidgetTree(
  toRef(props, 'externalId'),
  rootElementWithFallback,
  toRef(props, 'conditionalPorts'),
  extended,
  anyLayoutAnimationActive,
  primaryApplication,
)
watch(toRef(tree, 'currentEdit'), (edit) => emit('currentEditChanged', edit))
</script>
<script lang="ts">
export const GRAB_HANDLE_X_MARGIN_L = 4
export const GRAB_HANDLE_X_MARGIN_R = 8
export const ICON_WIDTH = 16
</script>

<template>
  <div ref="treeRoot">
    <WidgetTreeRootStyles
      class="WidgetTreeRoot widgetRounded"
      spellcheck="false"
      v-on="layoutTransitions.events"
    >
      <NodeWidget :input="input" :updateCallback="updateCallback" />
    </WidgetTreeRootStyles>
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
</style>
