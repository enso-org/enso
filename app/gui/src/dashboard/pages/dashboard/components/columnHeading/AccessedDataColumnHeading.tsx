/** @file A heading for the "Accessed data" column. */
import { Button } from '#/components/Button'
import { Text } from '#/components/Text'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import type { AssetColumnHeadingProps } from '#/pages/dashboard/components/column'
import { Column } from '#/pages/dashboard/components/column/columnUtils'
import { useText } from '$/providers/react'

/** A heading for the "Accessed data" column. */
export default function AccessedDataColumnHeading(props: AssetColumnHeadingProps) {
  const { hideColumn } = props
  const { getText } = useText()

  const hideThisColumn = useEventCallback(() => {
    hideColumn(Column.accessedData)
  })

  return (
    <div className="flex h-table-row w-full items-center gap-2">
      <Button
        variant="icon"
        icon="accessed_data"
        aria-label={getText('accessedDataColumnHide')}
        tooltip={false}
        onPress={hideThisColumn}
      />
      <Text weight="bold" truncate="1" color="custom">
        {getText('accessedDataColumnName')}
      </Text>
    </div>
  )
}
