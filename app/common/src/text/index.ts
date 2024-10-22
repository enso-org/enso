/** @file Functions related to displaying text. */

import ENGLISH from './english.json' assert { type: 'json' }

// =============
// === Types ===
// =============

/** Possible languages in which to display text. */
export enum Language {
  english = 'english',
}

export const LANGUAGE_TO_LOCALE: Record<Language, string> = {
  [Language.english]: 'en-US',
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
  readonly restoreProjectError: [projectName: string]
  readonly unknownThreadIdError: [threadId: string]
  readonly needsOwnerError: [assetType: string]
  readonly inviteSuccess: [userEmail: string]
  readonly inviteManyUsersSuccess: [userCount: number]

  readonly deleteLabelActionText: [labelName: string]
  readonly deleteSelectedAssetActionText: [assetName: string]
  readonly deleteSelectedAssetsActionText: [count: number]
  readonly deleteSelectedAssetForeverActionText: [assetName: string]
  readonly deleteSelectedAssetsForeverActionText: [count: number]
  readonly deleteUserActionText: [userName: string]
  readonly deleteUserGroupActionText: [groupName: string]
  readonly removeUserFromUserGroupActionText: [userName: string, groupName: string]
  readonly confirmPrompt: [action: string]
  readonly trashTheAssetTypeTitle: [assetType: string, assetName: string]
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
  readonly removeTheLocalDirectoryXFromFavorites: [directoryName: string]
  readonly upgradeTo: [planName: string]
  readonly enterTheNewKeyboardShortcutFor: [actionName: string]
  readonly downloadProjectError: [projectName: string]
  readonly downloadFileError: [fileName: string]
  readonly downloadDatalinkError: [datalinkName: string]
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
  readonly restoreProjectBackendError: [string]
  readonly duplicateProjectBackendError: [string]
  readonly closeProjectBackendError: [string]
  readonly listProjectSessionsBackendError: [string]
  readonly getProjectDetailsBackendError: [string]
  readonly getProjectLogsBackendError: [string]
  readonly openProjectBackendError: [string]
  readonly openProjectMissingCredentialsBackendError: [string]
  readonly updateProjectBackendError: [string]
  readonly checkResourcesBackendError: [string]
  readonly uploadFileWithNameBackendError: [string]
  readonly getFileDetailsBackendError: [string]
  readonly createDatalinkBackendError: [string]
  readonly getDatalinkBackendError: [string]
  readonly deleteDatalinkBackendError: [string]
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
  readonly logEventBackendError: [string]

  readonly subscribeSuccessSubtitle: [string]
  readonly assetsDropFilesDescription: [count: number]

  readonly paywallAvailabilityLevel: [plan: string]
  readonly paywallScreenDescription: [plan: string]
  readonly userGroupsLimitMessage: [limit: number]
  readonly inviteFormSeatsLeftError: [exceedBy: number]
  readonly inviteFormSeatsLeft: [seatsLeft: number]
  readonly seatsLeft: [seatsLeft: number, seatsTotal: number]

  readonly userCategory: [userName: string]
  readonly teamCategory: [teamName: string]
  readonly userCategoryButtonLabel: [userName: string]
  readonly teamCategoryButtonLabel: [teamName: string]
  readonly userCategoryDropZoneLabel: [userName: string]
  readonly teamCategoryDropZoneLabel: [teamName: string]

  readonly upgradeCTA: [plan: string]
  readonly priceTemplate: [price: string, interval: string]
  readonly months: [months: number]
  readonly teamPlanSeatsDescription: [seats: number]
  readonly tryFree: [days: number]
  readonly organizationNameSettingsInputDescription: [howLong: number]
  readonly trialDescription: [days: number]
  readonly groupNameSettingsInputDescription: [howLong: number]
  readonly xIsUsingTheProject: [userName: string]
  readonly xItemsCopied: [count: number]
  readonly xItemsCut: [count: number]

  readonly arbitraryFieldTooLarge: [maxSize: string]
  readonly arbitraryFieldTooSmall: [minSize: string]
  readonly uploadLargeFileStatus: [uploadedParts: number, totalParts: number]
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
