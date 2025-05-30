/** @file A heading for the "Accessed by projects" column. */
import { Button } from '#/components/Button'
import { Text } from '#/components/Text'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import type { AssetColumnHeadingProps } from '#/pages/dashboard/components/column'
import { Column } from '#/pages/dashboard/components/column/columnUtils'
import { useText } from '$/providers/react'

/** A heading for the "Accessed by projects" column. */
export default function AccessedByProjectsColumnHeading(props: AssetColumnHeadingProps) {
  const { hideColumn } = props
  const { getText } = useText()

  const hideThisColumn = useEventCallback(() => {
    hideColumn(Column.accessedByProjects)
  })

  return (
    <div className="flex h-table-row w-full items-center gap-2">
      <Button
        variant="icon"
        icon="accessed_by_projects"
        aria-label={getText('accessedByProjectsColumnHide')}
        tooltip={false}
        onPress={hideThisColumn}
      />
      <Text weight="bold" truncate="1" color="custom">
        {getText('accessedByProjectsColumnName')}
      </Text>
    </div>
  )
}
