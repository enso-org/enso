import { splitFilename } from '@/components/widgets/FileBrowserWidget/nameBar'
import { AssetType } from 'enso-common/src/services/Backend'
import { computed, ref, toValue, watch, type WatchSource } from 'vue'

export interface Asset {
  title: string
  type: AssetType
}

export interface PredefinedFilter {
  type: 'predefined'
  label: string
  extensions: string[]
}

export interface GlobFilter {
  type: 'glob'
}

export interface UserInputFilter {
  type: 'userInput'
  input: string
}

export type Filter = PredefinedFilter | GlobFilter | UserInputFilter

/** A model for file extension filter dropdown. */
export function useFileExtensionFilter(
  filenameInputContents: WatchSource<string>,
  fileExtensionInputContents: WatchSource<string>,
) {
  const filter = ref<Filter>({
    type: 'glob',
  })

  const hasExtension = computed(() => {
    const dotIndex = toValue(filenameInputContents).lastIndexOf('.')
    return dotIndex !== -1
  })

  watch(
    fileExtensionInputContents,
    (value) => {
      if (value === '*' || value === '') {
        filter.value = {
          type: 'glob',
        }
      } else {
        filter.value = {
          type: 'userInput',
          // The only supported glob pattern is *, so if it is not the only character, we remove it.
          input: value.replaceAll('*', ''),
        }
      }
    },
    { flush: 'sync' },
  )

  function matches(asset: Asset): boolean {
    if (asset.type !== AssetType.file) return true
    if (filter.value.type === 'glob') return true
    const [_, extension] = splitFilename(asset.title)
    if (filter.value.type === 'userInput') {
      return extension.startsWith(filter.value.input)
    }
    return filter.value.extensions.some((ext) => extension === ext)
  }

  const filenameSuffix = computed(() => {
    if (hasExtension.value) return ''
    if (filter.value.type === 'glob') return ''
    if (filter.value.type === 'userInput') return '.' + filter.value.input
    return '.' + filter.value.extensions[0]
  })
  const displayedExtension = computed(() => {
    if (filter.value.type === 'glob') return '*'
    if (filter.value.type === 'userInput') return filter.value.input
    return filter.value.label
  })

  return { matches, filter, filenameSuffix, displayedExtension }
}

export type FileExtensionFilter = ReturnType<typeof useFileExtensionFilter>
