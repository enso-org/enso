<script setup lang="ts">
import { type Component, reactive } from 'vue'

const _props = defineProps<{
  host: VueHostInstance
}>()
</script>

<script lang="ts">
let nextId = 0

/**
 * Supports creation of Vue Components within a particular Vue context.
 *
 * This enables creating Vue Components from code run outside any Vue context by APIs that render custom HTML content
 * but aren't Vue-aware.
 */
export interface VueHost {
  /**
   * Request the given component to begin being rendered as a child of the specified HTML element. The returned
   * handle allows updating and unregistering the component.
   */
  register(
    component: Component,
    element: HTMLElement,
    customKey?: string | symbol,
  ): VueComponentHandle
}

export interface VueComponentHandle {
  unregister: () => void
  update: (component: Component, element: HTMLElement) => void
}

/**
 * Implements the {@link VueHost} API supporting registration of components to be rendered with Vue.
 *
 * To render registered components, the VueHostInstance object should be passed to VueHostRender component.
 */
export class VueHostInstance implements VueHost {
  readonly teleportations = reactive(new Map<number | string | symbol, [Component, HTMLElement]>())

  /* eslint-disable-next-line jsdoc/require-jsdoc */
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
</script>

<template>
  <template v-for="[key, [component, slot]] in host.teleportations.entries()" :key="key">
    <Teleport :to="slot">
      <component :is="component" />
    </Teleport>
  </template>
</template>
