<script setup lang="ts">
import { useObjectId } from 'enso-common/src/utilities/data/object'
import { type Component, reactive } from 'vue'

const teleportations = reactive(new Map<number | string | symbol, [Component, HTMLElement]>())

let nextId = 0

defineExpose({
  register: (component: Component, element: HTMLElement, customKey?: string | symbol) => {
    const key = customKey ?? nextId++
    teleportations.set(key, [component, element])
    return {
      unregister: () => teleportations.delete(key),
      update: (component: Component, element: HTMLElement) =>
        teleportations.set(key, [component, element]),
    }
  },
} satisfies VueHost)
</script>

<script lang="ts">
/**
 * Supports creation of Vue Components within a particular Vue context.
 *
 * This enables creating Vue Components from code run outside any Vue context by APIs that render custom HTML content
 * but aren't Vue-aware.
 */
export interface VueHost {
  /**
   * Request the given component to begin being rendered as a child of the specified HTML element. The returned
   * `unregister` function should be called when the component should no longer be rendered.
   */
  register: (component: Component, element: HTMLElement) => VueComponentHandle
}

export interface VueComponentHandle {
  unregister: () => void
  update: (component: Component, element: HTMLElement) => void
}
</script>

<template>
  <template v-for="[key, [component, slot]] in teleportations.entries()" :key="key">
    <Teleport :to="slot">
      <component :is="component" />
    </Teleport>
  </template>
</template>
