/** @file Types for columns. */

/** Column type. */
export enum Column {
  name = 'name',
  modified = 'modified',
  sharedWith = 'sharedWith',
  labels = 'labels',
  path = 'path',
  accessedByProjects = 'accessedByProjects',
  accessedData = 'accessedData',
}

/** Columns that can be used as a sort column. */
export type SortableColumn = Column.modified | Column.name
