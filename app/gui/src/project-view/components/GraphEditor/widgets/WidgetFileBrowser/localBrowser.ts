import { type BrowserItem } from '@/components/GraphEditor/widgets/WidgetFileBrowser/browsableTypes'
import { type CustomDropdownItem } from '@/components/GraphEditor/widgets/WidgetSelection/tags'
import { assert } from '@/util/assert'
import { type ToValue } from '@/util/reactivity'
import { computed, toValue, type ComputedRef } from 'vue'

const LABELS = new Map<BrowserItem, string>([
  ['directory', 'Choose directory…'],
  ['file', 'Choose file…'],
])

/** @returns Dropdown items for opening a local file browser. */
export function useLocalBrowser({
  dialogKind,
  write,
  currentPath,
  setPath,
}: {
  dialogKind: ToValue<BrowserItem>
  write: ToValue<boolean>
  currentPath: ToValue<string | undefined>
  setPath: (type: 'file', path: string) => void
}): ComputedRef<CustomDropdownItem[]> {
  async function openFileBrowser() {
    if (!window.fileBrowserApi) {
      console.error('File browser not supported!')
      return
    }

    const rawKind = toValue(dialogKind)
    assert(rawKind !== 'secret')
    const kind = rawKind === 'file' && toValue(write) ? 'filePath' : rawKind
    const selected = await window.fileBrowserApi.openFileBrowser(kind, toValue(currentPath))
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
