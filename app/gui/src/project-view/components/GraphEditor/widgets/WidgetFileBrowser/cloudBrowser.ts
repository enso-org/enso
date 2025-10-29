import { type FileType } from '$/providers/openedProjects/widgetRegistry/configuration'
import { type BrowserItem } from '@/components/GraphEditor/widgets/WidgetFileBrowser/browsableTypes'
import {
  type Actions,
  type CustomDropdownItem,
} from '@/components/GraphEditor/widgets/WidgetSelection/tags'
import FileBrowserWidget from '@/components/widgets/FileBrowserWidget.vue'
import { type Icon } from '@/util/iconMetadata/iconName'
import { type ToValue } from '@/util/reactivity'
import { computed, type ComputedRef, h, toValue } from 'vue'

const TYPES = new Map<BrowserItem, { label: string; icon?: Icon }>([
  ['directory', { label: 'Choose directory in cloud…' }],
  ['file', { label: 'Choose file in cloud…' }],
  ['secret', { label: 'Choose secret in cloud…', icon: 'key' }],
])

/** @returns Dropdown items for opening a cloud file browser. */
export function useCloudBrowser({
  dialogKind,
  write,
  currentPath,
  setPath,
  fileTypes,
}: {
  dialogKind: ToValue<BrowserItem>
  write: ToValue<boolean>
  currentPath: ToValue<string | undefined>
  setPath: (type: 'file' | 'secret', path: string) => void
  fileTypes: ToValue<FileType[] | undefined>
}): ComputedRef<CustomDropdownItem[]> {
  function openCloudBrowser({ setActivity, close }: Actions) {
    setActivity(
      computed(() => {
        const type = toValue(dialogKind)
        return h(FileBrowserWidget, {
          type,
          writeMode: toValue(write),
          choosenPath: toValue(currentPath) ?? '',
          onPathAccepted: (path: string) => {
            setPath(type === 'secret' ? 'secret' : 'file', path)
            close()
          },
          fileTypes: toValue(fileTypes),
          onClose: close,
        })
      }),
    )
  }

  const typeItem = new Map(
    [...TYPES.entries()].map(([key, { label, icon }]) => [
      key,
      {
        label,
        icon,
        onClick: openCloudBrowser,
      },
    ]),
  )

  return computed((): CustomDropdownItem[] => {
    const item = typeItem.get(toValue(dialogKind))
    return item ? [item] : []
  })
}
