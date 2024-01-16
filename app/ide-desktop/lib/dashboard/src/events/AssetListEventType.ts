/** @file Possible types of changes to the file list. */

/** Possible types of changes to the file list. */
enum AssetListEventType {
    newFolder = 'new-folder',
    newProject = 'new-project',
    uploadFiles = 'upload-files',
    newDataConnector = 'new-data-connector',
    closeFolder = 'close-folder',
    copy = 'copy',
    move = 'move',
    willDelete = 'will-delete',
    delete = 'delete',
    removeSelf = 'remove-self',
}

// This is REQUIRED, as `export default enum` is invalid syntax.
// eslint-disable-next-line no-restricted-syntax
export default AssetListEventType
