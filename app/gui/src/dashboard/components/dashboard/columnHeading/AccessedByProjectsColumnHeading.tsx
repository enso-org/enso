/** @file A heading for the "Accessed by projects" column. */
import AccessedByProjectsIcon from '#/assets/accessed_by_projects.svg'
import { Button, Text } from '#/components/AriaComponents'
import type { AssetColumnHeadingProps } from '#/components/dashboard/column'
import { Column } from '#/components/dashboard/column/columnUtils'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useText } from '#/providers/TextProvider'

/** A heading for the "Accessed by projects" column. */
export default function AccessedByProjectsColumnHeading(props: AssetColumnHeadingProps) {
  const { hideColumn } = props
  const { getText } = useText()

  const hideThisColumn = useEventCallback(() => {
    hideColumn(Column.accessedByProjects)
  })

  return (
    <div className="flex h-table-row w-full items-center gap-icon-with-text">
      <Button
        variant="icon"
        icon={AccessedByProjectsIcon}
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
