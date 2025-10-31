import {
  isExtensions,
  isFileTypes,
  type FileType,
} from '$/providers/openedProjects/widgetRegistry/configuration'
import { type BrowserItem } from '@/components/GraphEditor/widgets/WidgetFileBrowser/browsableTypes'
import { type CustomDropdownItem } from '@/components/GraphEditor/widgets/WidgetSelection/tags'
import { assert } from '@/util/assert'
import type { FileFilter } from '@/util/fileFilter'
import type { ToValue } from '@/util/reactivity'
import { computed, toValue, type ComputedRef } from 'vue'

const LABELS = new Map<BrowserItem, string>([
  ['directory', 'Choose directory…'],
  ['file', 'Choose file…'],
])

function fileTypesToFileFilters(fileTypes: FileType[]): FileFilter[] {
  return fileTypes.flatMap((fileType) => {
    const name = fileType.label
    if (fileType.extensions.length > 0) {
      if (isFileTypes(fileType.extensions)) {
        return fileTypesToFileFilters(fileType.extensions)
      } else if (isExtensions(fileType.extensions)) {
        return [{ name, extensions: fileType.extensions }]
      }
    }
    return []
  })
}

/** @returns Dropdown items for opening a local file browser. */
export function useLocalBrowser({
  dialogKind,
  write,
  currentPath,
  setPath,
  fileTypes,
}: {
  dialogKind: ToValue<BrowserItem>
  write: ToValue<boolean>
  currentPath: ToValue<string | undefined>
  setPath: (type: 'file', path: string) => void
  fileTypes: ToValue<FileType[] | undefined>
}): ComputedRef<CustomDropdownItem[]> {
  async function openFileBrowser() {
    if (!window.api) {
      console.error('File browser not supported!')
      return
    }

    const rawKind = toValue(dialogKind)
    assert(rawKind !== 'secret')
    const kind = rawKind === 'file' && toValue(write) ? 'filePath' : rawKind
    const fileTypes_ = toValue(fileTypes)
    const filters = fileTypes_ != null ? fileTypesToFileFilters(fileTypes_) : undefined
    const selected = await window.api.fileBrowser.openFileBrowser(
      kind,
      toValue(currentPath),
      filters,
    )
    if (selected != null && selected[0] != null) setPath('file', selected[0])
  }

  return computed(() => {
    const label = LABELS.get(toValue(dialogKind))
    return label ?
        [
          {
            label,
            onClick: openFileBrowser,
          },
        ]
      : []
  })
}
