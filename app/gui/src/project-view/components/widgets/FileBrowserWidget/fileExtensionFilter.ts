import { AssetType } from '#/services/Backend'
import { computed, ref, toValue, watch, WatchSource } from 'vue'

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

  watch(fileExtensionInputContents, (value) => {
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
  })

  function matches(asset: Asset): boolean {
    if (asset.type !== AssetType.file) return true
    if (filter.value.type === 'glob') return true
    if (filter.value.type === 'userInput') {
      return asset.title.endsWith(filter.value.input)
    }
    return filter.value.extensions.some((extension) => asset.title.endsWith(extension))
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
