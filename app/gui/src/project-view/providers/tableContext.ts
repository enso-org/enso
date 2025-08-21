import { createContextStore } from '@/providers'
import { identity } from '@vueuse/core'
import type { Ref } from 'vue'
import type { ExternalId } from 'ydoc-shared/yjsModel'

export const [provideTableContext, useTableContext] = createContextStore(
  'Table context',
  identity<Readonly<Ref<ExternalId | undefined>>>,
)
