/** @file A heading for the "Accessed by projects" column. */
import { Button, Text } from '#/components/AriaComponents'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useText } from '#/providers/TextProvider'
import type { AssetColumnHeadingProps } from '../columnProps'
import { Column } from '../types'

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
