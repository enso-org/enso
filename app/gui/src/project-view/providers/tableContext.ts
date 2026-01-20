import { createContextStore } from '@/providers'
import type { Ref } from 'vue'
import type { ExternalId } from 'ydoc-shared/yjsModel'

/** Information about the Enso value of type Table that is relevant to the current context. */
interface TableContext {
  /** Identifier of the table expression for use in backend queries. */
  externalId: Readonly<Ref<ExternalId | undefined>>
}

export const [provideTableContext, useTableContext] = createContextStore(
  'Table context',
  (externalId: Ref<ExternalId | undefined>): TableContext => {
    return {
      externalId,
    }
  },
)
