/** @file Possible types of asset state change. */

// ======================
// === AssetEventType ===
// ======================

/** Possible types of asset state change. */
enum AssetEventType {
  newProject = 'new-project',
  newFolder = 'new-folder',
  uploadFiles = 'upload-files',
  updateFiles = 'update-files',
  newDatalink = 'new-datalink',
  newSecret = 'new-secret',
  copy = 'copy',
  cut = 'cut',
  cancelCut = 'cancel-cut',
  move = 'move',
  delete = 'delete',
  deleteForever = 'delete-forever',
  restore = 'restore',
  download = 'download',
  downloadSelected = 'download-selected',
  removeSelf = 'remove-self',
  temporarilyAddLabels = 'temporarily-add-labels',
  temporarilyRemoveLabels = 'temporarily-remove-labels',
  addLabels = 'add-labels',
  removeLabels = 'remove-labels',
  deleteLabel = 'delete-label',
  projectClosed = 'project-closed',
}

export default AssetEventType
