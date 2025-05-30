/** @file A heading for the "Labels" column. */
import { Button } from '#/components/Button'
import { Text } from '#/components/Text'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import type { AssetColumnHeadingProps } from '#/pages/dashboard/components/column'
import { Column } from '#/pages/dashboard/components/column/columnUtils'
import { useText } from '$/providers/react'

/** A heading for the "Labels" column. */
export default function LabelsColumnHeading(props: AssetColumnHeadingProps) {
  const { hideColumn } = props

  const { getText } = useText()

  const hideThisColumn = useEventCallback(() => {
    hideColumn(Column.labels)
  })

  return (
    <div className="isolate flex h-table-row w-full items-center gap-2">
      <Button
        variant="icon"
        icon="tag"
        aria-label={getText('labelsColumnHide')}
        tooltip={false}
        onPress={hideThisColumn}
      />
      <Text weight="bold" truncate="1" color="custom">
        {getText('labelsColumnName')}
      </Text>
    </div>
  )
}
