import { createContextStore } from '@/providers'
import { computedFallback } from '@/util/reactivity'
import { defineTabButtons, ExtractTabs } from '@/util/tabs'
import { computed, proxyRefs, ref, toRef } from 'vue'
import { assertNever } from 'ydoc-shared/util/assert'
import { unwrapOr } from 'ydoc-shared/util/data/result'
import { GraphStore } from './graph'
import { PersistedStore } from './persisted'
import { useSettings } from './settings'

export type RightDockStore = ReturnType<typeof useRightDock>

export type RightDockTab = ExtractTabs<typeof tabButtons>
export const { buttons: tabButtons, isValidTab } = defineTabButtons([
  { tab: 'docs', icon: 'text', title: 'Documentation Editor' },
  { tab: 'help', icon: 'help', title: 'Component Help' },
])

export enum StorageMode {
  Default,
  ComponentBrowser,
}

export const [provideRightDock, useRightDock] = createContextStore(
  'rightDock',
  (graph: GraphStore, persisted: PersistedStore) => {
    const inspectedAst = computed(() => unwrapOr(graph.methodAst, undefined))
    const inspectedMethodPointer = computed(() => unwrapOr(graph.currentMethodPointer, undefined))
    const { user: userSettings } = useSettings()

    const storageMode = ref(StorageMode.Default)
    const markdownDocs = computed(() => inspectedAst.value?.mutableDocumentationMarkdown())

    const defaultVisible = computedFallback(
      toRef(persisted, 'graphRightDock'),
      () => (markdownDocs.value?.length ?? 0) > 0,
    )

    const defaultTab = computed<RightDockTab>({
      get: () => {
        const fromStorage = persisted.graphRightDockTab
        return fromStorage && isValidTab(fromStorage) ? fromStorage : 'docs'
      },
      set: (value) => (persisted.graphRightDockTab = value),
    })

    const width = toRef(persisted, 'graphRightDockWidth')

    const cbVisible = ref(true)
    const cbTab = ref<RightDockTab>('help')

    const displayedTab = computed<RightDockTab>(() => {
      switch (storageMode.value) {
        case StorageMode.Default:
          return defaultTab.value
        case StorageMode.ComponentBrowser:
          return (
            userSettings.value.showHelpForCB ? 'help'
            : defaultVisible.value ? defaultTab.value
            : cbTab.value
          )
        default:
          return assertNever(storageMode.value)
      }
    })

    function switchToTab(tab: RightDockTab) {
      switch (storageMode.value) {
        case StorageMode.Default:
          defaultTab.value = tab
          break
        case StorageMode.ComponentBrowser:
          cbTab.value = tab
          userSettings.value.showHelpForCB = tab === 'help'
          if (defaultVisible.value) defaultTab.value = tab
          break
        default:
          return assertNever(storageMode.value)
      }
    }

    const visible = computed(() => {
      switch (storageMode.value) {
        case StorageMode.Default:
          return defaultVisible.value
        case StorageMode.ComponentBrowser:
          return userSettings.value.showHelpForCB || cbVisible.value || defaultVisible.value
        default:
          return assertNever(storageMode.value)
      }
    })

    function setVisible(newVisible: boolean) {
      switch (storageMode.value) {
        case StorageMode.Default:
          defaultVisible.value = newVisible
          break
        case StorageMode.ComponentBrowser:
          cbVisible.value = newVisible
          userSettings.value.showHelpForCB = newVisible
          if (!newVisible) defaultVisible.value = false
          break
        default:
          return assertNever(storageMode.value)
      }
    }

    /** Show specific tab if it is not visible. Otherwise, close the right dock. */
    function toggleVisible(specificTab?: RightDockTab | undefined) {
      if (specificTab == null || displayedTab.value == specificTab) {
        setVisible(!visible.value)
      } else {
        switchToTab(specificTab)
        setVisible(true)
      }
    }

    return proxyRefs({
      markdownDocs,
      displayedTab,
      inspectedAst,
      inspectedMethodPointer,
      width,
      visible,
      setStorageMode(mode: StorageMode) {
        storageMode.value = mode
      },
      switchToTab,
      setVisible,
      toggleVisible,
    })
  },
)
