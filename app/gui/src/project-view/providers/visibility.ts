import { createContextStore } from '@/providers'
import { identity } from '@vueuse/core'
import { type Ref } from 'vue'

export { injectFn as injectVisibility, provideFn as provideVisibility }
const { provideFn, injectFn } = createContextStore('Visibility', identity<Ref<boolean>>)
