import { useFileExtensionFilter } from '@/components/widgets/FileBrowserWidget/fileExtensionFilter'
import { computed, ref } from 'vue'

/** Name bar of the FileBrowserWidget. */
export function useNameBar() {
  const filenameInput = ref('')
  const extensionInput = ref('')
  const fileExtensionFilter = useFileExtensionFilter(filenameInput, extensionInput)

  const fullFilePath = computed(() => {
    return filenameInput.value === '' ?
        ''
      : `${filenameInput.value}${fileExtensionFilter.filenameSuffix.value}`
  })

  return {
    filenameInput,
    extensionInput,
    fullFilePath,
    fileExtensionFilter,
    setFilename: (filename: string) => {
      const [name, extension] = splitFilename(filename)
      // Filtering datalinks does not make much sense, and we should probably change behavior for other extensions as well in the future.
      if (extension === 'datalink') {
        filenameInput.value = filename
      } else {
        filenameInput.value = name
        extensionInput.value = extension
      }
    },
  }
}

/** Extract filename and extension parts from a full filename. */
export function splitFilename(filename: string): [string, string] {
  const dotIndex = filename.lastIndexOf('.')
  if (dotIndex === -1) return [filename, '']
  return [filename.slice(0, dotIndex), filename.slice(dotIndex + 1)]
}
