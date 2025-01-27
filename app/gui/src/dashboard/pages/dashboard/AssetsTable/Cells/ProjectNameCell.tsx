/**
 * @file Project name cell
 */

import EditableSpan from '#/components/EditableSpan'
import { useText } from '#/providers/TextProvider'
import { twJoin } from '#/utilities/tailwindMerge'
import type { AssetsTableCellContext } from './types'
import { getNameSchema } from './utilities'

/**
 * Props for {@link ProjectNameCell}
 */
export interface ProjectNameCellProps {
  readonly state: AssetsTableCellContext
}

/**
 * Project name cell
 */
export function ProjectNameCell(props: ProjectNameCellProps) {
  const { state } = props

  const title = state.cell.getValue()

  const { getText } = useText()

  return (
    <EditableSpan
      data-testid="asset-row-name"
      editable={state.isEditingName}
      className={twJoin(
        'grow bg-transparent font-naming',
        canExecute && !isOtherUserUsingProject && 'cursor-pointer',
      )}
      onSubmit={doRename}
      onCancel={() => {
        setIsEditing(false)
      }}
      schema={() => {
        return getNameSchema({ itemId: state.item.id, nodeMap: state.nodeMap, getText })
      }}
    >
      {title}
    </EditableSpan>
  )
}
