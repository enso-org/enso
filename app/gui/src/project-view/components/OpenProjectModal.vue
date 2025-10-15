<script setup lang="ts">
import { unsetModal } from '#/providers/ModalProvider'
import { EnsoPath } from '#/services/Backend'
import { useContainerData } from '$/providers/container'
import StandaloneButton from '@/components/StandaloneButton.vue'
import { injectInteractionHandler, type Interaction } from '@/providers/interactionHandler'
import { computed, onMounted } from 'vue'

const props = defineProps<{ href: string }>()

const path = computed(() => EnsoPath(props.href))
const containerData = useContainerData()
const interaction = injectInteractionHandler()

const modalInteraction: Interaction = { cancel: unsetModal, end: unsetModal }

function closeModal() {
  interaction.end(modalInteraction)
}

onMounted(() => {
  interaction.setCurrent(modalInteraction)
})

function upsertTab() {
  containerData.tab = path.value
}
</script>

<template>
  <teleport to="#floatingLayer">
    <div class="OpenProjectModal" @keydown.esc="closeModal" @mousedown.self.prevent="closeModal">
      <div class="modal-container" @mousedown.stop.prevent>
        <h2>Open Project</h2>
        <p>
          Would you like to open the project at '{{ decodeURIComponent(href) }}'? The current
          project will be closed.
        </p>
        <div class="button-bar">
          <StandaloneButton label="Cancel" @activate="closeModal" />
          <StandaloneButton label="Open" variant="submit" @activate="upsertTab" />
        </div>
      </div>
    </div>
  </teleport>
</template>

<style scoped>
.OpenProjectModal {
  position: absolute;
  top: 0;
  left: 0;
  width: 100vw;
  height: 100vh;
  cursor: pointer;
  max-height: 100vh;
  display: grid;
  place-items: center;

  &::before {
    content: '';
    position: absolute;
    inset: 0;
    top: -100px;
    height: calc(100% + 200px);
    background: var(--color-dim);
  }

  > .modal-container {
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
