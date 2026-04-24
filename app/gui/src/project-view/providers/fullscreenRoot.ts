import { createContextStore } from '@/providers'
import { identity } from '@vueuse/core'
import type { Ref } from 'vue'

export const [provideFullscreenRoot, useFullscreenRoot] = createContextStore(
  'fullscreenRoot',
  identity<Ref<HTMLElement | undefined | null>>,
)
