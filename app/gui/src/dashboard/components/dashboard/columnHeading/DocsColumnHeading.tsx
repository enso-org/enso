/** @file A heading for the "Docs" column. */
import DocsIcon from '#/assets/docs.svg'
import { Button, Text } from '#/components/AriaComponents'
import type { AssetColumnHeadingProps } from '#/components/dashboard/column'
import { Column } from '#/components/dashboard/column/columnUtils'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useText } from '#/providers/TextProvider'

/** A heading for the "Docs" column. */
export default function DocsColumnHeading(props: AssetColumnHeadingProps) {
  const { hideColumn } = props
  const { getText } = useText()

  const hideThisColumn = useEventCallback(() => {
    hideColumn(Column.docs)
  })

  return (
    <div className="isolate flex h-table-row w-full items-center gap-icon-with-text">
      <Button
        variant="icon"
        icon={DocsIcon}
        aria-label={getText('docsColumnHide')}
        tooltip={false}
        onPress={hideThisColumn}
      />
      <Text weight="bold" truncate="1" color="custom">
        {getText('docsColumnName')}
      </Text>
    </div>
  )
}
