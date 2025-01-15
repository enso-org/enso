import { createContextStore } from '@/providers'
import { identity } from '@vueuse/core'
import { type Ref } from 'vue'

export const [provideVisibility, injectVisibility] = createContextStore(
  'Visibility',
  identity<Ref<boolean>>,
)
