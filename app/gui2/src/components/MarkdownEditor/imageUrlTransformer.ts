import { createContextStore } from '@/providers'
import type { ToValue } from '@/util/reactivity'
import { toValue } from 'vue'
import { mapOk, Ok, type Result } from 'ydoc-shared/util/data/result'

export type TransformUrlResult = Result<{ url: string; dispose?: () => void }>
export type UrlTransformer = (url: string) => Promise<TransformUrlResult>

export {
  injectFn as injectLexicalImageUrlTransformer,
  provideFn as provideLexicalImageUrlTransformer,
}
const { provideFn, injectFn } = createContextStore(
  'Lexical image URL transformer',
  (transformUrl: ToValue<UrlTransformer | undefined>) => ({
    transformUrl: (url: string) => toValue(transformUrl)?.(url),
  }),
)

type ResourceId = string
type Url = string
export interface ResourceInfo<T> {
  location: T
  uniqueId: ResourceId
}
export type ResourceLocator<T> = (url: Url) => Promise<Result<ResourceInfo<T>> | undefined>
export type ResourceFetcher<T> = (locator: T) => Promise<Result<Blob>>

/** TODO: Add docs */
export function fetcherUrlTransformer<ResourceLocation>(
  locateResource: ResourceLocator<ResourceLocation>,
  fetchResource: ResourceFetcher<ResourceLocation>,
): UrlTransformer {
  const allocatedUrls = new Map<ResourceId, Promise<Result<{ value: Url; refs: number }>>>()

  function startFetch(uniqueId: ResourceId, location: ResourceLocation) {
    const fetching = fetchResource(location).then((fetched) =>
      mapOk(fetched, (fetched) => ({ value: URL.createObjectURL(fetched), refs: 0 })),
    )
    allocatedUrls.set(uniqueId, fetching)
    return fetching
  }

  return async (url: Url) => {
    const resource = await locateResource(url)
    if (resource == null) {
      return Ok({ url })
    } else if (!resource.ok) {
      return resource
    } else {
      const { uniqueId, location } = resource.value
      const result = await (allocatedUrls.get(uniqueId) ?? startFetch(uniqueId, location))
      if (!result.ok) {
        // Changes to external state may allow a future attempt to succeed.
        allocatedUrls.delete(uniqueId)
        return result
      }
      result.value.refs += 1
      return Ok({
        url: result.value.value,
        dispose: () => {
          if (!(result.value.refs -= 1)) URL.revokeObjectURL(result.value.value)
        },
      })
    }
  }
}
