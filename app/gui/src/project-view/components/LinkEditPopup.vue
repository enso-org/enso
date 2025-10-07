<script setup lang="ts">
import { vueBackendQueryOptions } from '#/hooks/backendHooks'
import { setModal } from '#/providers/ModalProvider'
import { AssetType } from '#/services/Backend'
import { vueComponent } from '#/utilities/vue'
import { useBackends } from '$/providers/backends'
import { useContainerData } from '$/providers/container'
import { useRightPanelData } from '$/providers/rightPanel'
import { textEditorsBindings } from '@/bindings'
import OpenProjectModal from '@/components/OpenProjectModal.vue'
import { useOpenProjectLocally } from '@/composables/project'
import { autoUpdate, flip, useFloating } from '@floating-ui/vue'
import { useQuery } from '@tanstack/vue-query'
import { BackendType, EnsoPath } from 'enso-common/src/services/Backend'
import { createElement } from 'react'
import { computed, toRef, useTemplateRef, watchEffect } from 'vue'

const props = defineProps<{
  referenceElement: HTMLElement
  href: string
  popOut: boolean
}>()

const containerData = useContainerData()

const path = computed(() => EnsoPath(props.href))

const rightPanelData = useRightPanelData()
const openProjectLocally = useOpenProjectLocally()
const { backendForType } = useBackends()
const backendType = computed(() => rightPanelData.context?.category?.backend ?? BackendType.remote)
const resolveEnsoPathQuery = useQuery(
  vueBackendQueryOptions(backendForType(backendType.value), 'resolveEnsoPath', [path]),
)
const assetQuery = useQuery(
  vueBackendQueryOptions(
    backendForType(backendType.value),
    'getAssetDetails',
    [
      // This is UNSAFE, but `enabled` below ensures that this query will not run if the ID is undefined.
      // eslint-disable-next-line @typescript-eslint/no-non-null-asserted-optional-chain
      computed(() => resolveEnsoPathQuery.data.value?.id!),
      undefined,
    ],
    {
      enabled() {
        // QueryKey: [backendType.value, 'getAssetDetails', resolveEnsoPathQuery.data.value?.id]
        return !!this.queryKey?.[2]
      },
    },
  ),
)

const isProject = computed(() => rightPanelData.focusedAsset?.type === AssetType.project)

const shouldOpenProjectModal = computed(() => {
  if (!isProject.value) return false
  if (containerData.openedProjects.some((project) => project.ensoPath === path.value)) {
    // The project is already opened.
    return false
  }
  for (const [otherPath] of containerData.openingProjects.values()) {
    // The project is in the process of being opened.
    if (otherPath === path.value) return false
  }
  return true
})

const OpenProjectModalReact = vueComponent(OpenProjectModal).default

const floatingElement = useTemplateRef<HTMLElement>('floating')

function openProjectModal() {
  setModal(createElement(OpenProjectModalReact, { href: props.href }))
}

async function openProjectInNewTab() {
  const maybeProject = await assetQuery.promise.value
  if (maybeProject?.type !== AssetType.project) return
  openProjectLocally({ ...maybeProject, ensoPath: path.value }, backendType.value)
}

const { floatingStyles } = useFloating(toRef(props, 'referenceElement'), floatingElement, {
  placement: 'top-start',
  strategy: () => (props.popOut ? 'fixed' : 'absolute'),
  middleware: [flip()],
  whileElementsMounted: autoUpdate,
})

watchEffect(() => {
  console.log(shouldOpenProjectModal.value, isProject.value, props.href)
})
</script>

<template>
  <teleport to="#floatingLayer">
    <div ref="floating" class="LinkEditPopup" :style="floatingStyles" @pointerdown.stop.prevent>
      <a v-if="shouldOpenProjectModal" class="link" @click="openProjectModal">Follow link</a>
      <a v-else-if="isProject" class="link" @click="openProjectInNewTab">Follow link</a>
      <a v-else class="link" :href="href" target="_blank" rel="noopener,noreferrer">Follow link</a>
      ({{ textEditorsBindings.bindings.openLink.humanReadable }})
    </div>
  </teleport>
</template>

<style scoped>
.LinkEditPopup {
  font-family: var(--font-sans);
  color: gray;
  border-radius: var(--radius-default);
  backdrop-filter: var(--blur-app-bg);
  background-color: rgba(255, 255, 255, 0.9);
  box-shadow: 0 4px 30px rgba(0, 0, 0, 0.2);
  padding: 8px;
  width: max-content;
}

.link {
  cursor: pointer;
  color: blue;

  &:hover {
    text-decoration: underline;
  }
}

.modal {
  position: absolute;
  top: 0;
  left: 0;
  width: 100vw;
  height: 100vh;
  cursor: pointer;
  max-height: 100vh;

  &::before {
    content: '';
    position: absolute;
    inset: 0;
    top: -100px;
    height: calc(100% + 200px);
    background: var(--color-dim);
  }

  > .modal-container {
    position: absolute;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    background-color: var(--color-app-bg);
    border-radius: var(--radius-default);
    backdrop-filter: blur(8px);
    cursor: default;
    padding: 1em;
    display: flex;
    flex-flow: column nowrap;
    gap: 1em;
    max-width: 32em;
  }
}

.button-bar {
  display: flex;
  gap: 1em;
  justify-content: flex-end;
}
</style>
