/** @file Functions related to displaying text. */

import ENGLISH from './text/english.json' with { type: 'json' }
import { unsafeKeys } from './utilities/data/object'

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
  readonly deleteTheAssetTypeTitleForever: [assetType: string, assetName: string]
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
  readonly projectSessionX: [count: number]
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

  readonly inviteUserBackendError: [userEmail: string]
  readonly changeUserGroupsBackendError: [userName: string]
  readonly listFolderBackendError: [folderTitle: string]
  readonly createFolderBackendError: [folderTitle: string]
  readonly updateFolderBackendError: [folderTitle: string]
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
  readonly xAm: [hour: string]
  readonly xPm: [hour: string]
  readonly everyHourXMinute: [minute: string]
  readonly groupNameSettingsInputDescription: [howLong: number]
  readonly xIsUsingTheProject: [userName: string]
  readonly xItemsCopied: [count: number]
  readonly xItemsCut: [count: number]
  readonly ordinalFallback: [number: number]
  readonly dateXTimeX: [date: string, time: string]
  readonly hourlyBetweenX: [startTime: string, endTime: string]
  readonly projectSessionsOnX: [date: string]
  readonly xthDayOfMonth: [dateOrdinal: string]
  readonly xthXDayOfMonth: [weekOrdinal: string, dayOfWeek: string]
  readonly lastXDayOfMonth: [dayOfWeek: string]
  readonly repeatsTimeXMonthsXDateX: [time: string, months: string, date: string]
  readonly repeatsTimeXMonthsXDayXWeekX: [time: string, months: string, day: string, week: string]
  readonly repeatsTimeXMonthsXDayXLastWeek: [time: string, months: string, day: string]
  readonly xthWeek: [weekOrdinal: string]

  readonly arbitraryFieldTooLarge: [maxSize: string]
  readonly arbitraryFieldTooSmall: [minSize: string]
  readonly uploadLargeFileStatus: [uploadedParts: number, totalParts: number]

  readonly latestVersion: [version: string, date: string]
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
/**
 * A function that gets localized text for a given key, with optional replacements.
 * @param key - The key of the text to get.
 * @param replacements - The replacements to insert into the text.
 * If the text contains placeholders like `$0`, `$1`, etc.,
 * they will be replaced with the corresponding replacement.
 */
export type GetText = <K extends TextId>(
  dictionary: Texts,
  key: K,
  ...replacements: Replacements[K]
) => string

/**
 * Resolves the language texts based on the user's preferred language.
 */
export function resolveUserLanguage() {
  const locale = navigator.language
  const language =
    unsafeKeys(LANGUAGE_TO_LOCALE).find((language) => locale === LANGUAGE_TO_LOCALE[language]) ??
    Language.english

  return language
}

/**
 * Gets the dictionary for a given language.
 * @param language - The language to get the dictionary for.
 * @returns The dictionary for the given language.
 */
export function getDictionary(language: Language) {
  return TEXTS[language]
}

/**
 * Gets the text for a given key, with optional replacements.
 * @param dictionary - The dictionary to get the text from.
 * @param key - The key of the text to get.
 * @param replacements - The replacements to insert into the text.
 * If the text contains placeholders like `$0`, `$1`, etc.,
 * they will be replaced with the corresponding replacement.
 */
export const getText: GetText = (dictionary, key, ...replacements) => {
  const template = dictionary[key]

  return replacements.length === 0 ?
      template
    : template.replace(/[$]([$]|\d+)/g, (_match, placeholder: string) =>
        placeholder === '$' ? '$' : String(replacements[Number(placeholder)] ?? `$${placeholder}`),
      )
}
