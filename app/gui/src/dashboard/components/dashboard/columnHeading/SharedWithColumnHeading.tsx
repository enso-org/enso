/** @file A heading for the "Shared with" column. */
import { Button } from '#/components/Button'
import type { AssetColumnHeadingProps } from '#/components/dashboard/column'
import { Column } from '#/components/dashboard/column/columnUtils'
import { Text } from '#/components/Text'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useText } from '$/providers/react'

/** A heading for the "Shared with" column. */
export default function SharedWithColumnHeading(props: AssetColumnHeadingProps) {
  const { hideColumn } = props

  const { getText } = useText()

  const hideThisColumn = useEventCallback(() => {
    hideColumn(Column.sharedWith)
  })

  return (
    <div className="isolate flex h-table-row w-full items-center gap-2">
      <Button
        variant="icon"
        icon="people"
        aria-label={getText('sharedWithColumnHide')}
        tooltip={false}
        onPress={hideThisColumn}
      />

      <div className="flex items-center gap-1">
        <Text weight="bold" truncate="1" color="custom">
          {getText('sharedWithColumnName')}
        </Text>
      </div>
    </div>
  )
}
