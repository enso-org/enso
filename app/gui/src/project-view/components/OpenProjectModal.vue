<script setup lang="ts">
import { vueBackendQueryOptions } from '#/hooks/backendHooks'
import { unsetModal } from '#/providers/ModalProvider'
import { AssetType, BackendType, EnsoPath, ProjectId } from '#/services/Backend'
import { useBackends } from '$/providers/backends'
import { useRightPanelData } from '$/providers/rightPanel'
import StandaloneButton from '@/components/StandaloneButton.vue'
import { useOpenProjectLocally } from '@/composables/project'
import { injectInteractionHandler, type Interaction } from '@/providers/interactionHandler'
import { useQuery } from '@tanstack/vue-query'
import { AnimatePresence, motion } from 'motion-v'
import { computed, onMounted } from 'vue'

const props = defineProps<{ href: string }>()

const path = computed(() => EnsoPath(props.href))

const interaction = injectInteractionHandler()
const openProjectLocally = useOpenProjectLocally()
const rightPanelData = useRightPanelData()
const { backendForType } = useBackends()
const backendType = computed(() => rightPanelData.context?.category?.backend ?? BackendType.remote)
const resolveEnsoPathQuery = useQuery(
  vueBackendQueryOptions(backendForType(backendType.value), 'resolveEnsoPath', [path]),
)
const assetQuery = useQuery(
  vueBackendQueryOptions(backendForType(backendType.value), 'getAssetDetails', [
    computed(() => resolveEnsoPathQuery.data.value?.id as ProjectId),
    undefined,
  ]),
)

const modalInteraction: Interaction = {
  cancel() {
    console.log(':(', modalInteraction)
    unsetModal()
  },
  end() {
    console.log(':( 2', modalInteraction)
    unsetModal()
  },
}

function closeModal() {
  interaction.end(modalInteraction)
}

onMounted(() => {
  interaction.setCurrent(modalInteraction)
})

async function openProjectInNewTab() {
  closeModal()
  const maybeProject = await assetQuery.promise.value
  if (maybeProject?.type !== AssetType.project) return
  openProjectLocally({ ...maybeProject, ensoPath: path.value }, backendType.value)
}
</script>

<template>
  <teleport to="#floatingLayer">
    <AnimatePresence>
      <motion.div
        class="modal"
        :animate="{ opacity: 1, y: '0' }"
        @keydown.esc="closeModal"
        @mousedown.self.prevent="closeModal"
      >
        <div class="modal-container" @mousedown.stop.prevent>
          <h2>Open Project</h2>
          <p>
            Would you like to open the project at '{{ decodeURIComponent(href) }}'? The current
            project will be closed.
          </p>
          <div class="button-bar">
            <StandaloneButton label="Cancel" @activate="closeModal" />
            <StandaloneButton label="Open" variant="submit" @activate="openProjectInNewTab" />
          </div>
        </div>
      </motion.div>
    </AnimatePresence>
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
