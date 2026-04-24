import { blockTypeAction } from '@/components/MarkdownEditor/blockTypeActions'
import type { BlockType } from '@/components/MarkdownEditor/codemirror/formatting'
import SelectionDropdown from '@/components/SelectionDropdown.vue'
import type { SelectionMenuOption } from '@/components/visualizations/toolbar'
import type { Actions } from '@/providers/action'
import { bindModelValue } from '@/util/vueDom'
import { computed, type Ref, toValue } from 'vue'
import type { ComponentProps } from 'vue-component-type-helpers'

const blockTypesOrdered: BlockType[] = [
  'Paragraph',
  'ATXHeading1',
  'ATXHeading2',
  'ATXHeading3',
  'BulletList',
  'OrderedList',
  'Blockquote',
]

const UNKNOWN_BLOCK_TYPE_OPTION: SelectionMenuOption = {
  icon: 'text',
  label: 'Paragraph type',
  labelExtension: undefined,
  disabled: false,
  hidden: true,
}

/**
 * @returns the {@link SelectionDropdown} properties for a block type dropdown, if it should be
 * displayed.
 */
export function useBlockTypeDropdown({
  blockType,
  actions,
}: {
  blockType: Ref<BlockType | 'Unknown'>
  actions: Actions
}) {
  // Code cannot directly be converted to other block types. Switching to `Paragraph` removes the
  // delimiters, and allows whatever is contained to be interpreted as Markdown; once the content is
  // Markdown, further styling changes can be made.
  const disableSettingTypes = computed<boolean>(() => blockType.value === 'FencedCode')

  function menuOption(key: BlockType): SelectionMenuOption | undefined {
    const action = actions[blockTypeAction[key]]
    if (!toValue(action.available)) return
    const shortcut = toValue(action.shortcut)
    return {
      icon: toValue(action.icon),
      label: toValue(action.description),
      labelExtension: shortcut && `(${shortcut.humanReadable})`,
      disabled: disableSettingTypes.value ? key !== blockType.value && key !== 'Paragraph' : false,
      hidden: false,
    }
  }

  const standardOptions = computed((): [BlockType, SelectionMenuOption][] =>
    blockTypesOrdered.flatMap((key) => {
      const option = menuOption(key)
      return option ? [[key, option]] : []
    }),
  )
  const currentTypeOption = computed((): SelectionMenuOption | undefined =>
    blockType.value === 'Unknown' ? UNKNOWN_BLOCK_TYPE_OPTION
    : blockTypesOrdered.includes(blockType.value) ? undefined
    : menuOption(blockType.value),
  )
  const blockTypeOptions = computed(
    (): Record<string, SelectionMenuOption> =>
      Object.fromEntries(
        currentTypeOption.value ?
          [...standardOptions.value, [blockType.value, currentTypeOption.value]]
        : standardOptions.value,
      ),
  )

  return computed((): ComponentProps<typeof SelectionDropdown> | undefined =>
    standardOptions.value.length > 0 ?
      {
        ...bindModelValue<string>(blockType),
        options: blockTypeOptions.value,
        labelButton: true,
      }
    : undefined,
  )
}
