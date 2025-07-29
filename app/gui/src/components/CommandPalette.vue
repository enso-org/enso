<script setup lang="ts">
import { unsetModal } from '#/providers/ModalProvider'
import * as objects from '#/utilities/object'
import { useActionsStore } from '$/providers/actions'
import { useEvent } from '@/composables/events'
import { registerHandlers } from '@/providers/action'
import { AnimatePresence, motion } from 'motion-v'
import { computed, ref, watchEffect } from 'vue'
import { commandPaletteBindings } from '../project-view/bindings'

const { findActions } = useActionsStore()

const visible = ref(false)
const query = ref('')
const input = ref<HTMLInputElement | null>(null)

const actionHandlers = registerHandlers({
  'commandPalette.open': {
    action: () => {
      visible.value = true
    },
  },
  'commandPalette.close': {
    action: () => {
      visible.value = false
    },
  },
})

watchEffect(() => {
  if (!input.value) return
  if (visible.value) {
    unsetModal()
    input.value.focus()
  } else {
    input.value.blur()
    query.value = ''
  }
})

useEvent(
  window,
  'keydown',
  commandPaletteBindings.handler(
    objects.mapEntries(
      commandPaletteBindings.bindings,
      (actionName) => actionHandlers[actionName].action,
    ),
  ),
)

const actions = computed(() => findActions(query.value))
</script>

<template>
  <AnimatePresence>
    <motion.div
      v-if="visible"
      class="CommandPalette"
      :initial="{ opacity: 0, y: '-100px' }"
      :animate="{ opacity: 1, y: '0' }"
      :exit="{ opacity: 0, y: '-100px' }"
      @click.stop="visible = false"
    >
      <div class="container" @click.stop>
        <input ref="input" v-model="query" type="text" placeholder="Search actions..." />
        <div class="scroll-container">
          <ul>
            <li v-for="(action, i) in actions" :key="i">
              <!-- eslint-disable vue/no-v-html -->
              <button
                @click="((visible = false), action.doAction())"
                v-html="action.highlighted.name"
              ></button>
              <!-- eslint-enable -->
            </li>
            <li v-if="!actions.length" class="disabled">No actions found</li>
          </ul>
        </div>
      </div>
    </motion.div>
  </AnimatePresence>
</template>

<style scoped>
.CommandPalette {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  z-index: 2;
  cursor: pointer;
  font-size: 14px;
  color: rgba(0, 0, 0, 0.9);
  max-height: 100vh;

  &::before {
    content: '';
    position: absolute;
    inset: 0;
    top: -100px;
    height: calc(100% + 200px);
    background: rgba(0, 0, 0, 0.25);
  }
}

.container {
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

.scroll-container {
  overflow-y: auto;
  height: 20em;
  width: 100%;
  padding-right: 0.5em;
}

input {
  background: none;
  width: calc(100% + 2em);
  margin: 0 -1em;
  padding: 0 2em 0.5em 2em;
  border-bottom: 0.1px solid rgb(0 0 0 / 0.2);
}

li {
  color: var(--color-primary);
  border-radius: var(--radius-default);
  padding: 0.2em 1em;

  &:focus-within {
    background-color: var(--color-menu-entry-selected-bg);
  }

  &:not(.disabled):hover {
    background-color: var(--color-menu-entry-hover-bg);
  }

  button {
    width: 100%;
    text-align: left;
    border-radius: var(--radius-default);
  }
}

:deep(.highlighted) {
  color: var(--color-text);
  font-weight: bold;
}
</style>
