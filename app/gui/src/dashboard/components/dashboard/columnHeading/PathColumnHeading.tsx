/** @file A heading for the "Path" column. */
import DirectoryIcon from '#/assets/folder.svg'
import { Button, Text } from '#/components/AriaComponents'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useText } from '#/providers/TextProvider'
import type { AssetColumnHeadingProps } from '../column'
import { Column } from '../column/columnUtils'

/** A heading for the "Path" column. */
export default function PathColumnHeading(props: AssetColumnHeadingProps) {
  const { hideColumn } = props

  const { getText } = useText()

  const hideThisColumn = useEventCallback(() => {
    hideColumn(Column.path)
  })

  return (
    <div
      className="isolate flex h-table-row w-full items-center gap-icon-with-text"
      data-testid="path-column-heading"
    >
      <Button
        variant="icon"
        icon={DirectoryIcon}
        aria-label={getText('pathColumnHide')}
        tooltip={false}
        onPress={hideThisColumn}
      />
      <Text weight="bold" truncate="1" color="custom">
        {getText('pathColumnName')}
      </Text>
    </div>
  )
}
