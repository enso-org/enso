/**
 * @file Datalink name cell
 */

import EditableSpan from '#/components/EditableSpan'
import type { AnyCellProps } from './types'

/**
 * Props for {@link DatalinkNameCell}
 */
export interface DatalinkNameCellProps extends AnyCellProps {}

/**
 * Datalink name cell
 */
export function DatalinkNameCell(props: DatalinkNameCellProps) {
  const { state } = props
  return (
    <>
      <EditableSpan
        data-testid="asset-row-name"
        editable={rowState.isEditingName}
        className={tailwindMerge.twMerge(
          'grow bg-transparent font-naming',
          canExecute && !isOtherUserUsingProject && 'cursor-pointer',
          rowState.isEditingName && 'cursor-text',
        )}
        onSubmit={doRename}
        onCancel={() => {
          setIsEditing(false)
        }}
        schema={(z) =>
          z.refine(
            (value) =>
              backendModule.isNewTitleUnique(
                item,
                value,
                nodeMap.current.get(item.parentId)?.children?.map((child) => child.item),
              ),
            { message: getText('nameShouldBeUnique') },
          )
        }
      >
        {item.title}
      </EditableSpan>
    </>
  )
}
