import { identity } from '@vueuse/core'
import { Ref } from 'vue'
import { createContextStore } from '.'

export const [provideFullscreenRoot, useFullscreenRoot] = createContextStore(
  'fullscreenRoot',
  identity<Ref<HTMLElement | undefined | null>>,
)
