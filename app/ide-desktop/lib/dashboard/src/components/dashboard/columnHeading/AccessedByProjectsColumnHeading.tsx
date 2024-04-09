/** @file A heading for the "Accessed by projects" column. */
import * as React from 'react'

import AccessedByProjectsIcon from 'enso-assets/accessed_by_projects.svg'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import type * as column from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import SvgMask from '#/components/SvgMask'

/** A heading for the "Accessed by projects" column. */
export default function AccessedByProjectsColumnHeading(props: column.AssetColumnHeadingProps) {
  const { state } = props
  const { hideColumn } = state
  const { getText } = textProvider.useText()

  return (
    <div className="flex h-drive-table-heading w-full items-center gap-icon-with-text">
      <SvgMask
        src={AccessedByProjectsIcon}
        className="size-icon"
        title={getText('accessedByProjectsColumnHide')}
        onClick={event => {
          event.stopPropagation()
          hideColumn(columnUtils.Column.accessedByProjects)
        }}
      />
      <aria.Text className="text-header">{getText('accessedByProjectsColumnName')}</aria.Text>
    </div>
  )
}
