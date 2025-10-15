/** @file Actions for the context menu. */
import type BaseActions from './BaseActions'
import type { PageCallback } from './BaseActions'
import EditorPageActions from './EditorPageActions'
import { TEXT } from './utilities'

/** Actions for the context menu. */
export interface ContextMenuActions<T extends BaseActions<Context>, Context> {
  readonly open: () => T
  readonly rename: () => T
  readonly exportToCloud: () => T
  readonly exportToLocal: () => T
  readonly delete: () => T
  readonly moveToTrash: () => T
  readonly restoreFromTrash: () => T
  readonly share: () => T
  readonly label: () => T
  readonly duplicate: () => T
  readonly duplicateProject: () => EditorPageActions<Context>
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

/** Generate actions for the context menu. */
export function contextMenuActions<T extends BaseActions<Context>, Context>(
  step: (name: string, callback: PageCallback<Context, T>) => T,
): ContextMenuActions<T, Context> {
  return {
    open: () =>
      step('Open (context menu)', (page) =>
        page.getByRole('button', { name: TEXT.openShortcut }).getByText(TEXT.openShortcut).click(),
      ),
    rename: () =>
      step('Rename (context menu)', (page) =>
        page
          .getByRole('button', { name: TEXT.renameShortcut })
          .getByText(TEXT.renameShortcut)
          .click(),
      ),
    exportToCloud: () =>
      step('Export to cloud (context menu)', (page) =>
        page
          .getByRole('button', { name: TEXT.uploadToCloudShortcut })
          .getByText(TEXT.uploadToCloudShortcut)
          .click(),
      ),
    exportToLocal: () =>
      step('Export to local (context menu)', (page) =>
        page
          .getByRole('button', { name: TEXT.downloadToLocalShortcut })
          .getByText(TEXT.downloadToLocalShortcut)
          .click(),
      ),
    delete: () =>
      step('Delete (context menu)', async (page) => {
        await page
          .getByRole('button', { name: TEXT.deleteShortcut })
          .getByText(TEXT.deleteShortcut)
          // Click the first result; the second one is the text of the keybind.
          .first()
          .click()

        // Confirm the deletion in the dialog
        await page.getByRole('button', { name: TEXT.delete }).getByText(TEXT.delete).click()
      }),
    moveToTrash: () =>
      step('Move to trash (context menu)', async (page) => {
        await page
          .getByRole('button', { name: TEXT.moveToTrashShortcut })
          .getByText(TEXT.moveToTrashShortcut)
          .click()

        // Confirm the deletion in the dialog
        await page.getByRole('button', { name: TEXT.delete }).getByText(TEXT.delete).click()
      }),
    restoreFromTrash: () =>
      step('Restore from trash (context menu)', (page) =>
        page
          .getByRole('button', { name: TEXT.restoreFromTrashShortcut })
          .getByText(TEXT.restoreFromTrashShortcut)
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
      ).into(EditorPageActions<Context>),
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
