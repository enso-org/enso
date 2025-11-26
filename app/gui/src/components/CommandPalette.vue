<script setup lang="ts">
import { CATEGORIES } from '#/configurations/inputBindings'
import KeyboardShortcutReact from '#/pages/dashboard/components/KeyboardShortcut'
import { unsetModal } from '#/providers/ModalProvider'
import { isTextInputEvent } from '#/utilities/event'
import { useActionsStore, type Action } from '$/providers/actions'
import { useContainerData } from '$/providers/container'
import { useText } from '$/providers/text'
import { commandPaletteBindings } from '@/bindings'
import SvgIcon from '@/components/SvgIcon.vue'
import { useEvent } from '@/composables/events'
import { registerHandlers } from '@/providers/action'
import { injectInteractionHandler } from '@/providers/interactionHandler'
import { reactComponent } from '@/util/react'
import { mapEntries } from 'enso-common/src/utilities/data/object'
import { computed, ref, watchEffect } from 'vue'

const KeyboardShortcut = reactComponent(KeyboardShortcutReact)

const { findActions } = useActionsStore()
const containerData = useContainerData()
const interaction = injectInteractionHandler()
const { getText } = useText()

const visible = ref(false)
const query = ref('')
const container = ref<HTMLDivElement | null>(null)
const input = ref<HTMLInputElement | null>(null)

const actionHandlers = registerHandlers({
  'commandPalette.open': {
    action: () => {
      if (containerData.tab !== 'drive' && containerData.tab !== 'settings') return
      open()
    },
  },
})

const commandPaletteInteraction = {
  cancel() {
    visible.value = false
  },
  end() {
    visible.value = false
  },
}

function open() {
  visible.value = true
  unsetModal()
  interaction.setCurrent(commandPaletteInteraction)
}

function close() {
  interaction.end(commandPaletteInteraction)
  query.value = ''
}

// This must be in a `watchEffect` or else `input.value` will be `null`
// since the modal is only conditionally shown.
watchEffect(() => {
  if (visible.value) {
    input.value?.focus()
  } else {
    input.value?.blur()
  }
})

useEvent(
  window,
  'keydown',
  commandPaletteBindings.handler(
    mapEntries(commandPaletteBindings.bindings, (actionName) => actionHandlers[actionName].action),
  ),
)

function trigger(action: Action | undefined) {
  if (!action) return
  close()
  action.doAction()
}

const actions = computed(() => findActions(query))

const groupedActions = computed(() => {
  const actionsValue = actions.value
  return CATEGORIES.flatMap((category) => {
    const categoryName = getText(`${category}BindingCategory`)
    const actionsInThisCategory = actionsValue.filter((action) => action.category === categoryName)
    if (actionsInThisCategory.length === 0) {
      return []
    }
    return [
      {
        category: categoryName,
        actions: actionsInThisCategory,
      },
    ]
  })
})

function getActionsElements() {
  const containerValue = container.value
  if (!containerValue) return []
  return containerValue.querySelectorAll('.action-entry button')
}

function focusPreviousAction() {
  if (!document.activeElement) return
  const actions = getActionsElements()
  const index = document.activeElement ? [...actions].indexOf(document.activeElement) : -1
  const action = actions[index === -1 ? actions.length - 1 : index - 1]
  if (!(action instanceof HTMLButtonElement)) return
  action?.focus()
}

function focusNextAction() {
  if (!document.activeElement) return
  const actions = getActionsElements()
  const index = document.activeElement ? [...actions].indexOf(document.activeElement) : -1
  const action = actions[index === -1 ? 0 : index + 1]
  if (!(action instanceof HTMLButtonElement)) return
  action?.focus()
}

function focusInputOnTextEvent(event: KeyboardEvent) {
  if (!isTextInputEvent(event)) return
  event.stopPropagation()
  input.value?.focus()
}
</script>

<template>
  <div
    v-if="visible"
    class="CommandPalette"
    @click.stop="close"
    @keydown="focusInputOnTextEvent"
    @keydown.enter.stop
    @keydown.arrow-up.prevent="focusPreviousAction"
    @keydown.arrow-down.prevent="focusNextAction"
  >
    <div ref="container" class="container" @click.stop>
      <input
        ref="input"
        v-model="query"
        type="text"
        placeholder="Search actions..."
        @keydown.enter.prevent="trigger(actions[0])"
      />
      <div class="scroll-container">
        <div v-for="actionsGroup in groupedActions" :key="actionsGroup.category">
          <h3 class="category-heading">{{ actionsGroup.category }}</h3>
          <ul>
            <li v-for="(action, i) in actionsGroup.actions" :key="i" class="action-entry">
              <button @click="trigger(action)">
                <SvgIcon v-if="action.icon" :name="action.icon" class="icon" />
                <div v-else class="icon-placeholder"></div>
                <!-- eslint-disable vue/no-v-html -->
                <span class="entry-content" v-html="action.highlighted.name"></span>
                <!-- eslint-enable -->
                <div class="shortcuts">
                  <KeyboardShortcut
                    v-for="(shortcut, j) in action.shortcuts"
                    :key="j"
                    :shortcut="shortcut"
                  />
                </div>
              </button>
            </li>
            <li v-if="!actions.length" class="disabled">No actions found</li>
          </ul>
        </div>
      </div>
    </div>
  </div>
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
  color: var(--color-text);
  max-height: 100vh;

  &::before {
    content: '';
    position: absolute;
    inset: 0;
    top: -100px;
    height: calc(100% + 200px);
    background: var(--color-dim);
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
  display: flex;
  flex-flow: column;
  overflow-y: auto;
  height: 20em;
  width: 100%;
  padding-right: 0.5em;
}

.category-heading {
  font-weight: bold;
  padding: 0.75em 1em 0.25em 1em;
}

input {
  background: none;
  width: calc(100% + 2em);
  margin: 0 -1em;
  padding: 0 2em 0.5em 2em;
  border-bottom: 0.1px solid rgb(0 0 0 / 0.2);
}

button {
  display: flex;
  gap: 0.75em;
  align-items: center;
}

.icon,
.icon-placeholder {
  display: inline-block;
  color: var(--color-text);
}

.icon-placeholder {
  width: 1em;
}

.entry-content {
  margin-right: auto;
}

.shortcuts {
  display: flex;
  gap: 0.5em;
  color: var(--color-text-secondary);
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
