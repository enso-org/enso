/** @file Actions for the context menu. */
import { TEXT } from '.'
import type * as baseActions from './BaseActions'
import type BaseActions from './BaseActions'
import EditorPageActions from './EditorPageActions'

// ==========================
// === ContextMenuActions ===
// ==========================

/** Actions for the context menu. */
export interface ContextMenuActions<T extends BaseActions> {
  readonly open: () => T
  readonly uploadToCloud: () => T
  readonly rename: () => T
  readonly snapshot: () => T
  readonly moveNonFolderToTrash: () => T
  readonly moveFolderToTrash: () => T
  readonly moveAllToTrash: () => T
  readonly restoreFromTrash: () => T
  readonly restoreAllFromTrash: () => T
  readonly share: () => T
  readonly label: () => T
  readonly duplicate: () => T
  readonly duplicateProject: () => EditorPageActions
  readonly copy: () => T
  readonly cut: () => T
  readonly paste: () => T
  readonly copyAsPath: () => T
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
  step: (name: string, callback: baseActions.PageCallback) => T,
): ContextMenuActions<T> {
  return {
    open: () =>
      step('Open (context menu)', (page) =>
        page.getByRole('button', { name: TEXT.openShortcut }).getByText(TEXT.openShortcut).click(),
      ),
    uploadToCloud: () =>
      step('Upload to cloud (context menu)', (page) =>
        page
          .getByRole('button', { name: TEXT.uploadToCloudShortcut })
          .getByText(TEXT.uploadToCloudShortcut)
          .click(),
      ),
    rename: () =>
      step('Rename (context menu)', (page) =>
        page
          .getByRole('button', { name: TEXT.renameShortcut })
          .getByText(TEXT.renameShortcut)
          .click(),
      ),
    snapshot: () =>
      step('Snapshot (context menu)', (page) =>
        page
          .getByRole('button', { name: TEXT.snapshotShortcut })
          .getByText(TEXT.snapshotShortcut)
          .click(),
      ),
    moveNonFolderToTrash: () =>
      step('Move to trash (context menu)', (page) =>
        page
          .getByRole('button', { name: TEXT.moveToTrashShortcut })
          .getByText(TEXT.moveToTrashShortcut)
          .click(),
      ),
    moveFolderToTrash: () =>
      step('Move folder to trash (context menu)', async (page) => {
        await page
          .getByRole('button', { name: TEXT.moveToTrashShortcut })
          .getByText(TEXT.moveToTrashShortcut)
          .click()
        await page.getByRole('button', { name: TEXT.delete }).getByText(TEXT.delete).click()
      }),
    moveAllToTrash: () =>
      step('Move all to trash (context menu)', (page) =>
        page
          .getByRole('button', { name: TEXT.moveAllToTrashShortcut })
          .getByText(TEXT.moveAllToTrashShortcut)
          .click(),
      ),
    restoreFromTrash: () =>
      step('Restore from trash (context menu)', (page) =>
        page
          .getByRole('button', { name: TEXT.restoreFromTrashShortcut })
          .getByText(TEXT.restoreFromTrashShortcut)
          .click(),
      ),
    restoreAllFromTrash: () =>
      step('Restore all from trash (context menu)', (page) =>
        page
          .getByRole('button', { name: TEXT.restoreAllFromTrashShortcut })
          .getByText(TEXT.restoreAllFromTrashShortcut)
          .click(),
      ),
    share: () =>
      step('Share (context menu)', (page) =>
        page
          .getByRole('button', { name: TEXT.shareShortcut })
          .getByText(TEXT.shareShortcut)
          .click(),
      ),
    label: () =>
      step('Label (context menu)', (page) =>
        page
          .getByRole('button', { name: TEXT.labelShortcut })
          .getByText(TEXT.labelShortcut)
          .click(),
      ),
    duplicate: () =>
      step('Duplicate (context menu)', (page) =>
        page
          .getByRole('button', { name: TEXT.duplicateShortcut })
          .getByText(TEXT.duplicateShortcut)
          .click(),
      ),
    duplicateProject: () =>
      step('Duplicate project (context menu)', (page) =>
        page
          .getByRole('button', { name: TEXT.duplicateShortcut })
          .getByText(TEXT.duplicateShortcut)
          .click(),
      ).into(EditorPageActions),
    copy: () =>
      step('Copy (context menu)', (page) =>
        page
          .getByRole('button', { name: TEXT.copyShortcut })
          .getByText(TEXT.copyShortcut, { exact: true })
          .click(),
      ),
    cut: () =>
      step('Cut (context menu)', (page) =>
        page.getByRole('button', { name: TEXT.cutShortcut }).getByText(TEXT.cutShortcut).click(),
      ),
    paste: () =>
      step('Paste (context menu)', (page) =>
        page
          .getByRole('button', { name: TEXT.pasteShortcut })
          .getByText(TEXT.pasteShortcut)
          .click(),
      ),
    copyAsPath: () =>
      step('Copy as path (context menu)', (page) =>
        page
          .getByRole('button', { name: TEXT.copyAsPathShortcut })
          .getByText(TEXT.copyAsPathShortcut)
          .click(),
      ),
    download: () =>
      step('Download (context menu)', (page) =>
        page
          .getByRole('button', { name: TEXT.downloadShortcut })
          .getByText(TEXT.downloadShortcut)
          .click(),
      ),
    // TODO: Specify the files in parameters.
    uploadFiles: () =>
      step('Upload files (context menu)', (page) =>
        page
          .getByRole('button', { name: TEXT.uploadFilesShortcut })
          .getByText(TEXT.uploadFilesShortcut)
          .click(),
      ),
    newFolder: () =>
      step('New folder (context menu)', (page) =>
        page
          .getByRole('button', { name: TEXT.newFolderShortcut })
          .getByText(TEXT.newFolderShortcut)
          .click(),
      ),
    newSecret: () =>
      step('New secret (context menu)', (page) =>
        page
          .getByRole('button', { name: TEXT.newSecretShortcut })
          .getByText(TEXT.newSecretShortcut)
          .click(),
      ),
    newDataLink: () =>
      step('New Data Link (context menu)', (page) =>
        page
          .getByRole('button', { name: TEXT.newDatalinkShortcut })
          .getByText(TEXT.newDatalinkShortcut)
          .click(),
      ),
  }
}
