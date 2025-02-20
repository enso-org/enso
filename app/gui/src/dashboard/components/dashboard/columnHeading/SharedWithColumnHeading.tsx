/** @file A heading for the "Shared with" column. */
import PeopleIcon from '#/assets/people.svg'
import { Button, Text } from '#/components/AriaComponents'
import type { AssetColumnHeadingProps } from '#/components/dashboard/column'
import { Column } from '#/components/dashboard/column/columnUtils'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useText } from '#/providers/TextProvider'

/** A heading for the "Shared with" column. */
export default function SharedWithColumnHeading(props: AssetColumnHeadingProps) {
  const { hideColumn } = props

  const { getText } = useText()

  const hideThisColumn = useEventCallback(() => {
    hideColumn(Column.sharedWith)
  })

  return (
    <div className="isolate flex h-table-row w-full items-center gap-icon-with-text">
      <Button
        variant="icon"
        icon={PeopleIcon}
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
