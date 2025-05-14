/** @file Column types and column display modes. */
import type { SvgUseIcon } from '#/components/AriaComponents'
import type { TextId } from 'enso-common/src/text'
import { memo } from 'react'
import type { AssetColumnHeadingProps, AssetColumnProps } from './columnProps'
import AccessedByProjectsColumnHeading from './headings/AccessedByProjectsColumnHeading'
import AccessedDataColumnHeading from './headings/AccessedDataColumnHeading'
import LabelsColumnHeading from './headings/LabelsColumnHeading'
import ModifiedColumnHeading from './headings/ModifiedColumnHeading'
import NameColumnHeading from './headings/NameColumnHeading'
import PathColumnHeading from './headings/PathColumnHeading'
import SharedWithColumnHeading from './headings/SharedWithColumnHeading'
import LabelsColumn from './LabelsColumn'
import ModifiedColumn from './ModifiedColumn'
import NameColumn from './NameColumn'
import PathColumn from './PathColumn'
import PlaceholderColumn from './PlaceholderColumn'
import SharedWithColumn from './SharedWithColumn'
import { Column } from './types'

/** React components for every column. */
export const COLUMN_RENDERER: Readonly<
  Record<Column, React.MemoExoticComponent<(props: AssetColumnProps) => React.JSX.Element>>
> = {
  [Column.name]: memo(NameColumn),
  [Column.modified]: memo(ModifiedColumn),
  [Column.sharedWith]: memo(SharedWithColumn),
  [Column.labels]: memo(LabelsColumn),
  [Column.accessedByProjects]: memo(PlaceholderColumn),
  [Column.accessedData]: memo(PlaceholderColumn),
  [Column.path]: memo(PathColumn),
}

/** React components for every column heading. */
export const COLUMN_HEADING: Readonly<
  Record<Column, React.MemoExoticComponent<(props: AssetColumnHeadingProps) => React.JSX.Element>>
> = {
  [Column.name]: memo(NameColumnHeading),
  [Column.modified]: memo(ModifiedColumnHeading),
  [Column.sharedWith]: memo(SharedWithColumnHeading),
  [Column.labels]: memo(LabelsColumnHeading),
  [Column.accessedByProjects]: memo(AccessedByProjectsColumnHeading),
  [Column.accessedData]: memo(AccessedDataColumnHeading),
  [Column.path]: memo(PathColumnHeading),
}

export const DEFAULT_ENABLED_COLUMNS: ReadonlySet<Column> = new Set([
  Column.name,
  Column.modified,
  Column.sharedWith,
  Column.labels,
  Column.path,
])

export const COLUMN_ICONS: Readonly<Record<Column, SvgUseIcon | (string & {})>> = {
  /* The file column does not have an icon, however this does not matter as it is not
   * collapsible. */
  [Column.name]: 'ghost',
  [Column.modified]: 'time',
  [Column.sharedWith]: 'people',
  [Column.labels]: 'tag',
  [Column.accessedByProjects]: 'accessed_by_projects',
  [Column.accessedData]: 'accessed_data',
  [Column.path]: 'folder',
}

export const COLUMN_SHOW_TEXT_ID: Readonly<Record<Column, TextId>> = {
  [Column.name]: 'nameColumnShow',
  [Column.modified]: 'modifiedColumnShow',
  [Column.sharedWith]: 'sharedWithColumnShow',
  [Column.labels]: 'labelsColumnShow',
  [Column.accessedByProjects]: 'accessedByProjectsColumnShow',
  [Column.accessedData]: 'accessedDataColumnShow',
  [Column.path]: 'pathColumnShow',
} satisfies { [C in Column]: `${C}ColumnShow` }

const COLUMN_CSS_CLASSES =
  'max-w-96 text-left bg-clip-padding last:border-r-0 last:rounded-r-full last:w-full last:min-w-0 last:max-w-0'
const NORMAL_COLUMN_CSS_CLASSES = `px-cell-x py ${COLUMN_CSS_CLASSES}`

/** CSS classes for every  */
export const COLUMN_CSS_CLASS: Readonly<Record<Column, string>> = {
  [Column.name]: `z-10 sticky left-1 bg-dashboard rounded-rows-skip-level w-80 max-w-80 h-full p-0 border-l-0 after:absolute after:right-0 after:top-0 after:bottom-0 after:border-r-[1.5px] after:border-primary/5 ${COLUMN_CSS_CLASSES}`,
  [Column.modified]: `min-w-drive-modified-column rounded-rows-have-level ${NORMAL_COLUMN_CSS_CLASSES}`,
  [Column.sharedWith]: `min-w-drive-shared-with-column rounded-rows-have-level ${NORMAL_COLUMN_CSS_CLASSES}`,
  [Column.labels]: `min-w-drive-labels-column rounded-rows-have-level ${NORMAL_COLUMN_CSS_CLASSES}`,
  [Column.accessedByProjects]: `min-w-drive-accessed-by-projects-column rounded-rows-have-level ${NORMAL_COLUMN_CSS_CLASSES}`,
  [Column.accessedData]: `min-w-drive-accessed-data-column rounded-rows-have-level ${NORMAL_COLUMN_CSS_CLASSES}`,
  [Column.path]: `min-w-drive-path-column rounded-rows-have-level ${NORMAL_COLUMN_CSS_CLASSES}`,
}
