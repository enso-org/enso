/** @file A heading for the "Docs" column. */
import DocsIcon from '#/assets/docs.svg'
import { Button, Text } from '#/components/AriaComponents'
import type { AssetColumnHeadingProps } from '#/components/dashboard/column'
import { Column } from '#/components/dashboard/column/columnUtils'
import { useText } from '#/providers/TextProvider'

/** A heading for the "Docs" column. */
export default function DocsColumnHeading(props: AssetColumnHeadingProps) {
  const { state } = props
  const { hideColumn } = state
  const { getText } = useText()

  return (
    <div className="flex h-table-row w-full items-center gap-icon-with-text">
      <Button
        variant="icon"
        icon={DocsIcon}
        aria-label={getText('docsColumnHide')}
        tooltip={false}
        onPress={() => {
          hideColumn(Column.docs)
        }}
      />
      <Text className="text-sm font-semibold">{getText('docsColumnName')}</Text>
    </div>
  )
}
