/** @file A heading for the "Labels" column. */
import * as React from 'react'

import TagIcon from 'enso-assets/tag.svg'

import * as columnUtils from '#/components/dashboard/column/columnUtils'
import SvgMask from '#/components/SvgMask'

/** A heading for the "Labels" column. */
export default function LabelsColumnHeading(): JSX.Element {
    return (
        <div className="flex items-center gap-2">
            <SvgMask src={TagIcon} className="h-4 w-4" />
            <span className="leading-144.5 h-6 py-0.5">
                {columnUtils.COLUMN_NAME[columnUtils.Column.labels]}
            </span>
        </div>
    )
}
