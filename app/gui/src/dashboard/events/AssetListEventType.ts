/** @file Possible types of changes to the file list. */

/** Possible types of changes to the file list. */
enum AssetListEventType {
  duplicateProject = 'duplicate-project',
  copy = 'copy',
  move = 'move',
  delete = 'delete',
  emptyTrash = 'empty-trash',
  removeSelf = 'remove-self',
}

export default AssetListEventType
