import { createContextStore } from '@/providers'
import { identity } from '@vueuse/core'
import type { ShallowRef } from 'vue'

export const [provideTopLevelArgument, useTopLevelArgument] = createContextStore(
  'Top-level argument',
  identity<Readonly<ShallowRef<HTMLElement | null>>>,
)
