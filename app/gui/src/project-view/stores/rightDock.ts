import { createContextStore } from '@/providers'
import { GraphStore } from '@/stores/graph'
import { PersistedStore } from '@/stores/persisted'
import { type ProjectStore } from '@/stores/project'
import { methodPointerEquals } from '@/util/methodPointer'
import { computedFallback } from '@/util/reactivity'
import { defineTabButtons, ExtractTabs } from '@/util/tabs'
import { computed, proxyRefs, toRef } from 'vue'
import { unwrapOr } from 'ydoc-shared/util/data/result'

export type RightDockStore = ReturnType<typeof useRightDock>

export type RightDockTab = ExtractTabs<typeof tabButtons>
export const { buttons: tabButtons, isValidTab } = defineTabButtons([
  { tab: 'docs', icon: 'document', title: 'Documentation Editor' },
  { tab: 'help', icon: 'help', title: 'Component Help' },
])

export const [provideRightDock, useRightDock] = createContextStore(
  'rightDock',
  (graph: GraphStore, project: ProjectStore, persisted: PersistedStore) => {
    const currentMethodAst = computed(() => unwrapOr(graph.currentMethod.ast, undefined))
    const inspectedMethodPointer = computed(() => unwrapOr(graph.currentMethod.pointer, undefined))
    const inspectedAst = computed(() =>
      (
        inspectedMethodPointer.value &&
        methodPointerEquals(inspectedMethodPointer.value, project.entryPoint)
      ) ?
        undefined
      : currentMethodAst.value,
    )

    const markdownDocs = computed(() => currentMethodAst.value?.mutableDocumentationMarkdown())

    const visible = computedFallback(
      toRef(persisted, 'graphRightDock'),
      () => (markdownDocs.value?.length ?? 0) > 0,
    )

    const displayedTab = computed<RightDockTab>({
      get: () => {
        const fromStorage = persisted.graphRightDockTab
        return fromStorage && isValidTab(fromStorage) ? fromStorage : 'docs'
      },
      set: (value) => (persisted.graphRightDockTab = value),
    })

    const width = toRef(persisted, 'graphRightDockWidth')

    /** Show specific tab if it is not visible. Otherwise, close the right dock. */
    function toggleVisible(specificTab?: RightDockTab | undefined) {
      if (specificTab == null || displayedTab.value == specificTab) {
        visible.value = !visible.value
      } else {
        displayedTab.value = specificTab
        visible.value = true
      }
    }

    return proxyRefs({
      markdownDocs,
      displayedTab,
      inspectedAst,
      inspectedMethodPointer,
      width,
      visible,
      toggleVisible,
    })
  },
)
