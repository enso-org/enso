/** @file Possible types of asset state change. */

// ======================
// === AssetEventType ===
// ======================

/** Possible types of asset state change. */
enum AssetEventType {
  newProject = 'new-project',
  newFolder = 'new-folder',
  uploadFiles = 'upload-files',
  newDataConnector = 'new-data-connector',
  openProject = 'open-project',
  closeProject = 'close-project',
  cancelOpeningAllProjects = 'cancel-opening-all-projects',
  copy = 'copy',
  cut = 'cut',
  cancelCut = 'cancel-cut',
  move = 'move',
  delete = 'delete',
  restore = 'restore',
  download = 'download',
  downloadSelected = 'download-selected',
  removeSelf = 'remove-self',
  temporarilyAddLabels = 'temporarily-add-labels',
  temporarilyRemoveLabels = 'temporarily-remove-labels',
  addLabels = 'add-labels',
  removeLabels = 'remove-labels',
  deleteLabel = 'delete-label',
}

// This is REQUIRED, as `export default enum` is invalid syntax.
// eslint-disable-next-line no-restricted-syntax
export default AssetEventType
