/** @file Functions related to displaying text. */

import ENGLISH from '#/text/english.json' assert { type: 'json' }

// =============
// === Types ===
// =============

/** Possible languages in which to display text. */
export enum Language {
  english = 'english',
}

/** An object containing the corresponding localized text for each text ID. */
type Texts = typeof ENGLISH
/** All possible text IDs. */
export type TextId = keyof Texts

/** Overrides the default number of placeholders (0). */
interface PlaceholderOverrides {
  readonly copyAssetError: [string]
  readonly moveAssetError: [string]
  readonly findProjectError: [string]
  readonly openProjectError: [string]
  readonly deleteAssetError: [string]
  readonly restoreAssetError: [string]
  readonly setPermissionsError: [string]
  readonly unknownThreadIdError: [string]
  readonly needsOwnerError: [string]
  readonly inviteSuccess: [string]

  readonly deleteLabelActionText: [string]
  readonly deleteSelectedAssetsActionText: [string, string]
  readonly confirmPrompt: [string]
  readonly deleteTheAssetTypeTitle: [string, string]
  readonly couldNotInviteUser: [string]
  readonly filesOrProjectsWithoutConflicts: [string, string]
  readonly filesAndProjectsWithoutConflicts: [string, string, string, string]
  readonly andOtherFilesOrProjects: [string, string]
  readonly andOtherFilesAndProjects: [string, string, string, string]
  readonly emailIsNotAValidEmail: [string]
  readonly userIsAlreadyInTheOrganization: [string]
  readonly youAreAlreadyAddingUser: [string]
  readonly lastModifiedOn: [string]
  readonly versionX: [string]
  readonly onDateX: [string]
  readonly xUsersSelected: [string]
  readonly upgradeTo: [string]
  readonly enterTheNewKeyboardShortcutFor: [string]
  readonly downloadProjectError: [string]
  readonly downloadFileError: [string]
  readonly downloadDataLinkError: [string]

  readonly inviteUserBackendError: [string]
  readonly listFolderBackendError: [string]
  readonly createFolderBackendError: [string]
  readonly updateFolderBackendError: [string]
  readonly listAssetVersionsBackendError: [string]
  readonly updateAssetBackendError: [string]
  readonly deleteAssetBackendError: [string]
  readonly undoDeleteAssetBackendError: [string]
  readonly copyAssetBackendError: [string, string]
  readonly createProjectBackendError: [string]
  readonly closeProjectBackendError: [string]
  readonly getProjectDetailsBackendError: [string]
  readonly openProjectBackendError: [string]
  readonly updateProjectBackendError: [string]
  readonly checkResourcesBackendError: [string]
  readonly uploadFileWithNameBackendError: [string]
  readonly getFileDetailsBackendError: [string]
  readonly createConnectorBackendError: [string]
  readonly getConnectorBackendError: [string]
  readonly deleteConnectorBackendError: [string]
  readonly createSecretBackendError: [string]
  readonly getSecretBackendError: [string]
  readonly updateSecretBackendError: [string]
  readonly createLabelBackendError: [string]
  readonly associateLabelsBackendError: [string]
  readonly deleteLabelBackendError: [string]
  readonly listVersionsBackendError: [string]
  readonly createCheckoutSessionBackendError: [string]
  readonly getCheckoutSessionBackendError: [string]
  readonly getDefaultVersionBackendError: [string]
}

/** An tuple of `string` for placeholders for each {@link TextId}. */
export interface Replacements
  extends PlaceholderOverrides,
    Record<Exclude<TextId, keyof PlaceholderOverrides>, []> {}

// =================
// === Constants ===
// =================

export const TEXTS: Readonly<Record<Language, Texts>> = {
  [Language.english]: ENGLISH,
}
