/** @file A heading for the "Accessed data" column. */
import AccessedDataIcon from '#/assets/accessed_data.svg'
import { Button, Text } from '#/components/AriaComponents'
import type { AssetColumnHeadingProps } from '#/components/dashboard/column'
import { Column } from '#/components/dashboard/column/columnUtils'
import { useText } from '#/providers/TextProvider'

/** A heading for the "Accessed data" column. */
export default function AccessedDataColumnHeading(props: AssetColumnHeadingProps) {
  const { state } = props
  const { hideColumn } = state
  const { getText } = useText()

  return (
    <div className="flex h-table-row w-full items-center gap-icon-with-text">
      <Button
        variant="icon"
        icon={AccessedDataIcon}
        aria-label={getText('accessedDataColumnHide')}
        tooltip={false}
        onPress={() => {
          hideColumn(Column.accessedData)
        }}
      />
      <Text className="text-sm font-semibold">{getText('accessedDataColumnName')}</Text>
    </div>
  )
}
