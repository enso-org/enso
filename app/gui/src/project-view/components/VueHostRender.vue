<script setup lang="ts">
import { type Component, reactive } from 'vue'

const _props = defineProps<{
  host: VueHost
}>()
</script>

<script lang="ts">
let nextId = 0

/**
 * Supports creation of Vue Components within a particular Vue context.
 *
 * This enables creating Vue Components from code run outside any Vue context by APIs that render custom HTML content
 * but aren't Vue-aware.
 *
 * To render registered components, the VueHost object should be passed to VueHostRender component.
 */
export class VueHost {
  readonly teleportations = reactive(new Map<number | string | symbol, [Component, HTMLElement]>())

  /**
   * Request the given component to begin being rendered as a child of the specified HTML element. The returned
   * handle allows updating and unregistering the component.
   */
  register(
    component: Component,
    element: HTMLElement,
    customKey?: string | symbol,
  ): VueComponentHandle {
    const key = customKey ?? nextId++
    this.teleportations.set(key, [component, element])
    return {
      unregister: () => this.teleportations.delete(key),
      update: (component: Component, element: HTMLElement) =>
        this.teleportations.set(key, [component, element]),
    }
  }
}

export interface VueComponentHandle {
  unregister: () => void
  update: (component: Component, element: HTMLElement) => void
}
</script>

<template>
  <template v-for="[key, [component, slot]] in host.teleportations.entries()" :key="key">
    <Teleport :to="slot">
      <component :is="component" />
    </Teleport>
  </template>
</template>
