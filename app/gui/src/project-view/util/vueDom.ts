import type { Ref } from 'vue'

/**
 * The type of event handlers for HTMLElement Events, in the naming scheme used by Vue.
 *
 * This can be used to generalize a `ComponentProps` type to allow event handlers passed-through to
 * the component's DOM element.
 */
export type HTMLElementEventHandler = {
  [K in keyof HTMLElementEventMap as `on${Capitalize<K>}`]?: (ev: HTMLElementEventMap[K]) => any
}

/** @returns the Vue Component props to bind a `Ref` to a `v-model`. */
export function bindModelValue<T>(modelValue: Ref<T>) {
  return {
    modelValue: modelValue.value,
    'onUpdate:modelValue': (value: T) => (modelValue.value = value),
  }
}
