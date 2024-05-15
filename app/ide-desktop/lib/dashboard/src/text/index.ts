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
  readonly copyAssetError: [assetName: string]
  readonly moveAssetError: [assetName: string]
  readonly findProjectError: [projectName: string]
  readonly openProjectError: [projectName: string]
  readonly deleteAssetError: [assetName: string]
  readonly restoreAssetError: [assetName: string]
  readonly unknownThreadIdError: [threadId: string]
  readonly needsOwnerError: [assetType: string]
  readonly inviteSuccess: [userEmail: string]

  readonly deleteLabelActionText: [labelName: string]
  readonly deleteSelectedAssetActionText: [assetName: string]
  readonly deleteSelectedAssetsActionText: [count: number]
  readonly deleteSelectedAssetForeverActionText: [assetName: string]
  readonly deleteSelectedAssetsForeverActionText: [count: number]
  readonly deleteUserActionText: [userName: string]
  readonly deleteUserGroupActionText: [groupName: string]
  readonly removeUserFromUserGroupActionText: [userName: string, groupName: string]
  readonly confirmPrompt: [action: string]
  readonly deleteTheAssetTypeTitle: [assetType: string, assetName: string]
  readonly couldNotInviteUser: [userEmail: string]
  readonly filesWithoutConflicts: [fileCount: number]
  readonly projectsWithoutConflicts: [projectCount: number]
  readonly andOtherFiles: [fileCount: number]
  readonly andOtherProjects: [projectCount: number]
  readonly emailIsNotAValidEmail: [userEmail: string]
  readonly userIsAlreadyInTheOrganization: [userEmail: string]
  readonly youAreAlreadyAddingUser: [userEmail: string]
  readonly lastModifiedOn: [dateString: string]
  readonly versionX: [version: number | string]
  readonly buildX: [build: string]
  readonly electronVersionX: [electronVersion: string]
  readonly chromeVersionX: [chromeVersion: string]
  readonly userAgentX: [userAgent: string]
  readonly compareVersionXWithLatest: [versionNumber: number]
  readonly onDateX: [dateString: string]
  readonly xUsersAndGroupsSelected: [usersAndGroupsCount: number]
  readonly upgradeTo: [planName: string]
  readonly enterTheNewKeyboardShortcutFor: [actionName: string]
  readonly downloadProjectError: [projectName: string]
  readonly downloadFileError: [fileName: string]
  readonly downloadDataLinkError: [dataLinkName: string]
  readonly deleteUserGroupError: [userGroupName: string]
  readonly removeUserFromUserGroupError: [userName: string, userGroupName: string]
  readonly deleteUserError: [userName: string]

  readonly inviteUserBackendError: [string]
  readonly changeUserGroupsBackendError: [string]
  readonly listFolderBackendError: [string]
  readonly createFolderBackendError: [string]
  readonly updateFolderBackendError: [string]
  readonly listAssetVersionsBackendError: [string]
  readonly getFileContentsBackendError: [string]
  readonly updateAssetBackendError: [string]
  readonly deleteAssetBackendError: [string]
  readonly undoDeleteAssetBackendError: [string]
  readonly copyAssetBackendError: [string, string]
  readonly createProjectBackendError: [string]
  readonly closeProjectBackendError: [string]
  readonly getProjectDetailsBackendError: [string]
  readonly openProjectBackendError: [string]
  readonly openProjectMissingCredentialsBackendError: [string]
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
  readonly createUserGroupBackendError: [string]
  readonly deleteUserGroupBackendError: [string]
  readonly listVersionsBackendError: [string]
  readonly createCheckoutSessionBackendError: [string]
  readonly getCheckoutSessionBackendError: [string]
  readonly getDefaultVersionBackendError: [string]

  readonly subscribeSuccessSubtitle: [string]
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
