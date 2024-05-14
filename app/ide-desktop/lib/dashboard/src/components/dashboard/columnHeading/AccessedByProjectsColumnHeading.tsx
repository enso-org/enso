/** @file A heading for the "Accessed by projects" column. */
import * as React from 'react'

import AccessedByProjectsIcon from 'enso-assets/accessed_by_projects.svg'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import type * as column from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import Button from '#/components/styled/Button'

/** A heading for the "Accessed by projects" column. */
export default function AccessedByProjectsColumnHeading(props: column.AssetColumnHeadingProps) {
  const { state } = props
  const { hideColumn } = state
  const { getText } = textProvider.useText()

  return (
    <div className="h-table-row flex w-full items-center gap-icon-with-text">
      <Button
        active
        image={AccessedByProjectsIcon}
        className="size-icon"
        alt={getText('accessedByProjectsColumnHide')}
        onPress={() => {
          hideColumn(columnUtils.Column.accessedByProjects)
        }}
      />
      <aria.Text className="text-header">{getText('accessedByProjectsColumnName')}</aria.Text>
    </div>
  )
}
