/** @file A heading for the "Accessed by projects" column. */
import AccessedByProjectsIcon from '#/assets/accessed_by_projects.svg'
import { Button, Text } from '#/components/AriaComponents'
import type { AssetColumnHeadingProps } from '#/components/dashboard/column'
import { Column } from '#/components/dashboard/column/columnUtils'
import { useText } from '#/providers/TextProvider'

/** A heading for the "Accessed by projects" column. */
export default function AccessedByProjectsColumnHeading(props: AssetColumnHeadingProps) {
  const { state } = props
  const { hideColumn } = state
  const { getText } = useText()

  return (
    <div className="flex h-table-row w-full items-center gap-icon-with-text">
      <Button
        variant="icon"
        icon={AccessedByProjectsIcon}
        aria-label={getText('accessedByProjectsColumnHide')}
        tooltip={false}
        onPress={() => {
          hideColumn(Column.accessedByProjects)
        }}
      />
      <Text className="text-sm font-semibold">{getText('accessedByProjectsColumnName')}</Text>
    </div>
  )
}
