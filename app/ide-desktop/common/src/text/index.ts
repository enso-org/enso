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
  readonly downloadDatalinkError: [datalinkName: string]
  readonly deleteUserGroupError: [userGroupName: string]
  readonly removeUserFromUserGroupError: [userName: string, userGroupName: string]
  readonly deleteUserError: [userName: string]

  readonly inviteUserBackendError: [userEmail: string]
  readonly changeUserGroupsBackendError: [userName: string]
  readonly listFolderBackendError: [folderTitle: string]
  readonly createFolderBackendError: [folderTitle: string]
  readonly updateFolderBackendError: [folderTitle: string]
  readonly listAssetVersionsBackendError: [assetTitle: string]
  readonly getFileContentsBackendError: [projectTitle: string]
  readonly updateAssetBackendError: [assetTitle: string]
  readonly deleteAssetBackendError: [assetTitle: string]
  readonly undoDeleteAssetBackendError: [assetTitle: string]
  readonly copyAssetBackendError: [assetTitle: string, newParentTitle: string]
  readonly createProjectBackendError: [projectTitle: string]
  readonly restoreProjectBackendError: [projectTitle: string]
  readonly duplicateProjectBackendError: [projectTitle: string]
  readonly closeProjectBackendError: [projectTitle: string]
  readonly listProjectSessionsBackendError: [projectTitle: string]
  readonly createProjectExecutionBackendError: [projectTitle: string]
  readonly updateProjectExecutionBackendError: [projectTitle: string]
  readonly deleteProjectExecutionBackendError: [projectTitle: string]
  readonly listProjectExecutionsBackendError: [projectTitle: string]
  readonly syncProjectExecutionBackendError: [projectTitle: string]
  readonly getProjectDetailsBackendError: [projectTitle: string]
  readonly getProjectLogsBackendError: [projectTitle: string]
  readonly openProjectBackendError: [projectTitle: string]
  readonly openProjectMissingCredentialsBackendError: [projectTitle: string]
  readonly updateProjectBackendError: [projectTitle: string]
  readonly checkResourcesBackendError: [projectTitle: string]
  readonly uploadFileWithNameBackendError: [fileTitle: string]
  readonly getFileDetailsBackendError: [fileTitle: string]
  readonly createDatalinkBackendError: [datalinkTitle: string]
  readonly getDatalinkBackendError: [datalinkTitle: string]
  readonly deleteDatalinkBackendError: [datalinkTitle: string]
  readonly createSecretBackendError: [secretTitle: string]
  readonly getSecretBackendError: [secretTitle: string]
  readonly updateSecretBackendError: [secretTitle: string]
  readonly createLabelBackendError: [labelName: string]
  readonly associateLabelsBackendError: [assetTitle: string]
  readonly deleteLabelBackendError: [labelName: string]
  readonly createUserGroupBackendError: [userGroupName: string]
  readonly deleteUserGroupBackendError: [userGroupName: string]
  readonly listVersionsBackendError: [versionType: string]
  readonly createCheckoutSessionBackendError: [plan: string]
  readonly getCheckoutSessionBackendError: [checkoutSessionId: string]
  readonly getDefaultVersionBackendError: [versionType: string]
  readonly logEventBackendError: [eventType: string]

  readonly subscribeSuccessSubtitle: [plan: string]
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

  readonly repeatsAtX: [dates: string]
  readonly xMinutes: [minutes: number]
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
