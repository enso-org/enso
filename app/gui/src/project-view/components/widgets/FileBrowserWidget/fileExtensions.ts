import {
  type FileType,
  isExtensions,
  isFileTypes,
  isGlobAll,
} from '$/providers/openedProjects/widgetRegistry/configuration'
import type SelectionSubmenu from '@/components/GraphEditor/widgets/WidgetSelection/SelectionSubmenu.vue'
import type { SubmenuComponent } from '@/components/GraphEditor/widgets/WidgetSelection/SelectionSubmenu.vue'
import type { SubmenuEntry } from '@/components/GraphEditor/widgets/WidgetSelection/submenuEntry'
import type AutoSizedInput from '@/components/widgets/AutoSizedInput.vue'
import type { Filter } from '@/components/widgets/FileBrowserWidget/fileExtensionFilter'
import { injectInteractionHandler, type Interaction } from '@/providers/interactionHandler'
import { endOnClick, targetIsOutside } from '@/util/autoBlur'
import type { ToValue } from '@/util/reactivity'
import type { HTMLElementEventHandler } from '@/util/vueDom'
import type { Opt } from 'enso-common/src/utilities/data/opt'
import { computed, type Ref, ref, toValue } from 'vue'
import type { ComponentProps } from 'vue-component-type-helpers'

interface FileExtensionEntry extends SubmenuEntry<FileExtensionEntry> {
  extensions: 'all' | string[]
}

interface Emit {
  (event: 'setFilter', filter: Filter): void
}

interface FileExtensionsOptions {
  filenameInput: Ref<string>
  extensionInput: Ref<string>
  fileExtensionInput: ToValue<Opt<InstanceType<typeof AutoSizedInput>>>
  fileExtensionInputRoot: ToValue<Opt<HTMLElement>>
  submenuRef: ToValue<Opt<SubmenuComponent>>
  fileExtensionFilter: ToValue<Filter>
  displayedExtension: ToValue<string>
  fileTypes: ToValue<ReadonlyArray<FileType>>
  root: ToValue<Opt<HTMLElement>>
  emit: Emit
}

/** Manages state of file-extension components. */
export function useFileExtensions({
  filenameInput,
  extensionInput,
  fileExtensionInput,
  fileExtensionInputRoot,
  submenuRef,
  fileExtensionFilter,
  displayedExtension,
  fileTypes,
  root,
  emit,
}: FileExtensionsOptions) {
  const interaction = injectInteractionHandler()
  const fileExtensionDropdownOpened = ref(false)

  const fileExtensionEntries = computed(() => toValue(fileTypes).map(fileTypeToFileExtensionEntry))

  function isSelected(value: string): boolean {
    const filter = toValue(fileExtensionFilter)
    if (filter.type === 'glob') return false
    if (filter.type === 'userInput') return filter.input === value
    return filter.label === value
  }

  function fileTypeToFileExtensionEntry(fileType: FileType): FileExtensionEntry {
    const nestedValues =
      isFileTypes(fileType.extensions) ? fileType.extensions.map(fileTypeToFileExtensionEntry) : []
    const extensions =
      isGlobAll(fileType.extensions) ? 'all'
      : isExtensions(fileType.extensions) ? fileType.extensions
      : []
    return {
      value: fileType.label,
      extensions,
      selected: isSelected(fileType.label),
      isNested: nestedValues.length > 0,
      nestedValues: nestedValues,
    }
  }

  function isOutsideDropdown(event: Event) {
    return toValue(submenuRef)?.isTargetOutside(event) ?? false
  }

  function isOutsideWidget(event: Event) {
    return targetIsOutside(event, toValue(root))
  }

  // Close the dropdown when clicking outside of it, but also end parent interaction (file browser widget) when clicking outside of both.
  const fileExtensionDropdownInteraction: Interaction = endOnClick(
    (event) => isOutsideDropdown(event) && !isOutsideWidget(event),
    {
      cancel: () => {
        fileExtensionDropdownOpened.value = false
      },
      end: () => {
        fileExtensionDropdownOpened.value = false
      },
      pointerdown: (event) => {
        if (
          isOutsideDropdown(event) &&
          isOutsideWidget(event) &&
          fileExtensionDropdownInteraction.parentInteraction
        ) {
          interaction.end(fileExtensionDropdownInteraction.parentInteraction)
        }
      },
    },
  )

  interaction.setWhenWithParent(
    () => fileExtensionDropdownOpened.value,
    (parentInteraction) => {
      fileExtensionDropdownInteraction.parentInteraction = parentInteraction
      return fileExtensionDropdownInteraction
    },
  )

  function openDropdown() {
    if (!fileExtensionDropdownOpened.value) {
      fileExtensionDropdownOpened.value = true
    }
    toValue(fileExtensionInput)?.select()
  }

  function extensionSelected(entry: FileExtensionEntry) {
    interaction.end(fileExtensionDropdownInteraction)
    if (filenameInput.value !== entry.value) {
      filenameInput.value = ''
    }
    if (entry.extensions === 'all' || entry.extensions.length === 0) {
      emit('setFilter', {
        type: 'glob',
      })
    } else {
      emit('setFilter', {
        type: 'predefined',
        label: entry.value,
        extensions: entry.extensions,
      })
    }
  }

  const fileExtensionInputModel = computed({
    get: () => {
      const filter = toValue(fileExtensionFilter)
      return filter.type === 'userInput' ? filter.input : toValue(displayedExtension)
    },
    set: (value) => {
      extensionInput.value = value
    },
  })

  const extensionInputField = computed(
    (): ComponentProps<typeof AutoSizedInput> & HTMLElementEventHandler => ({
      modelValue: toValue(fileExtensionInputModel),
      'onUpdate:modelValue': (value) => (fileExtensionInputModel.value = value),
      onClick: openDropdown,
    }),
  )

  const extensionMenu = computed(
    (): ComponentProps<typeof SelectionSubmenu<FileExtensionEntry>> => ({
      show: toValue(fileExtensionDropdownOpened),
      entries: toValue(fileExtensionEntries),
      topLevel: true,
      onClickedEntry: extensionSelected,
      floatReference: toValue(fileExtensionInputRoot),
    }),
  )

  return { extensionMenu, extensionInputField }
}
