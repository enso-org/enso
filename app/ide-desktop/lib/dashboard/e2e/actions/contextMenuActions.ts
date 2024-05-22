/** @file Actions for the context menu. */
import type * as baseActions from './BaseActions'
import type BaseActions from './BaseActions'

// ==========================
// === ContextMenuActions ===
// ==========================

/** Actions for the context menu. */
export interface ContextMenuActions<T extends BaseActions> {
  readonly open: () => T
  readonly uploadToCloud: () => T
  readonly rename: () => T
  readonly snapshot: () => T
  readonly moveToTrash: () => T
  readonly moveAllToTrash: () => T
  readonly restoreFromTrash: () => T
  readonly restoreAllFromTrash: () => T
  readonly share: () => T
  readonly label: () => T
  readonly duplicate: () => T
  readonly copy: () => T
  readonly cut: () => T
  readonly paste: () => T
  readonly download: () => T
  readonly uploadFiles: () => T
  readonly newFolder: () => T
  readonly newSecret: () => T
  readonly newDataLink: () => T
}

// ==========================
// === contextMenuActions ===
// ==========================

/** Generate actions for the context menu. */
export function contextMenuActions<T extends BaseActions>(
  step: (name: string, callback: baseActions.PageCallback) => T
): ContextMenuActions<T> {
  return {
    open: () =>
      step('Open (context menu)', page =>
        page.getByRole('button', { name: 'Open' }).getByText('Open').click()
      ),
    uploadToCloud: () =>
      step('Upload to cloud (context menu)', page =>
        page.getByRole('button', { name: 'Upload To Cloud' }).getByText('Upload To Cloud').click()
      ),
    rename: () =>
      step('Rename (context menu)', page =>
        page.getByRole('button', { name: 'Rename' }).getByText('Rename').click()
      ),
    snapshot: () =>
      step('Snapshot (context menu)', page =>
        page.getByRole('button', { name: 'Snapshot' }).getByText('Snapshot').click()
      ),
    moveToTrash: () =>
      step('Move to trash (context menu)', page =>
        page.getByRole('button', { name: 'Move To Trash' }).getByText('Move To Trash').click()
      ),
    moveAllToTrash: () =>
      step('Move all to trash (context menu)', page =>
        page
          .getByRole('button', { name: 'Move All To Trash' })
          .getByText('Move All To Trash')
          .click()
      ),
    restoreFromTrash: () =>
      step('Restore from trash (context menu)', page =>
        page
          .getByRole('button', { name: 'Restore From Trash' })
          .getByText('Restore From Trash')
          .click()
      ),
    restoreAllFromTrash: () =>
      step('Restore all from trash (context menu)', page =>
        page
          .getByRole('button', { name: 'Restore All From Trash' })
          .getByText('Restore All From Trash')
          .click()
      ),
    share: () =>
      step('Share (context menu)', page =>
        page.getByRole('button', { name: 'Share' }).getByText('Share').click()
      ),
    label: () =>
      step('Label (context menu)', page =>
        page.getByRole('button', { name: 'Label' }).getByText('Label').click()
      ),
    duplicate: () =>
      step('Duplicate (context menu)', page =>
        page.getByRole('button', { name: 'Duplicate' }).getByText('Duplicate').click()
      ),
    copy: () =>
      step('Copy (context menu)', page =>
        page.getByRole('button', { name: 'Copy' }).getByText('Copy').click()
      ),
    cut: () =>
      step('Cut (context menu)', page =>
        page.getByRole('button', { name: 'Cut' }).getByText('Cut').click()
      ),
    paste: () =>
      step('Paste (context menu)', page =>
        page.getByRole('button', { name: 'Paste' }).getByText('Paste').click()
      ),
    download: () =>
      step('Download (context menu)', page =>
        page.getByRole('button', { name: 'Download' }).getByText('Download').click()
      ),
    // TODO: Specify the files in parameters.
    uploadFiles: () =>
      step('Upload files (context menu)', page =>
        page.getByRole('button', { name: 'Upload Files' }).getByText('Upload Files').click()
      ),
    newFolder: () =>
      step('New folder (context menu)', page =>
        page.getByRole('button', { name: 'New Folder' }).getByText('New Folder').click()
      ),
    newSecret: () =>
      step('New secret (context menu)', page =>
        page.getByRole('button', { name: 'New Secret' }).getByText('New Secret').click()
      ),
    newDataLink: () =>
      step('New Data Link (context menu)', page =>
        page.getByRole('button', { name: 'New Data Link' }).getByText('New Data Link').click()
      ),
  }
}
