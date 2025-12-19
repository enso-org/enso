/** @file Functions related to displaying text. */
import ENGLISH from './text/english.json' with { type: 'json' }

/** Possible languages in which to display text. */
export type Language = 'english'

export const LANGUAGE_TO_LOCALE: Record<Language, string> = {
  english: 'en-US',
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
  readonly resolveDuplicatesTitleMany: [conflictingFileCount: number]
  readonly resolveDuplicatesDescriptionMany: [conflictingFileCount: number]
  readonly assetWillBeRenamed: [assetName: string]
  readonly andOtherFiles: [fileCount: number]
  readonly andOtherProjects: [projectCount: number]
  readonly emailIsNotAValidEmail: [userEmail: string]
  readonly userIsAlreadyInTheOrganization: [userEmail: string]
  readonly youAreAlreadyAddingUser: [userEmail: string]
  readonly lastModifiedOn: [dateString: string]
  readonly versionX: [version: number | string]
  readonly compareVersionXWithY: [versionNumber: number | string, versionNumber: string | number]
  readonly compareWithVersionY: [versionNumber: number | string]
  readonly projectSessionX: [count: number]
  readonly onDateX: [dateString: string]
  readonly byUserX: [userName: string]
  readonly xUsersAndGroupsSelected: [usersAndGroupsCount: number]
  readonly removeTheLocalDirectoryXFromFavorites: [directoryName: string]
  readonly upgradeTo: [planName: string]
  readonly enterTheNewKeyboardShortcutFor: [actionName: string]
  readonly downloadProjectError: [projectName: string]
  readonly downloadFileError: [fileName: string]
  readonly downloadDatalinkError: [datalinkName: string]
  readonly deleteUserGroupError: [userGroupName: string]
  readonly deleteUserError: [userName: string]

  readonly inviteUserBackendError: [userEmail: string]
  readonly changeUserGroupsBackendError: [userName: string]
  readonly listFolderBackendError: [folderTitle: string]
  readonly createFolderBackendError: [folderTitle: string]
  readonly updateFolderBackendError: [folderTitle: string]
  readonly updateAssetBackendError: [assetTitle: string]
  readonly deleteAssetBackendError: [assetTitle: string]
  readonly createProjectBackendError: [projectTitle: string]
  readonly duplicateProjectBackendError: [projectTitle: string]
  readonly closeProjectBackendError: [projectTitle: string]
  readonly listProjectSessionsBackendError: [projectTitle: string]
  readonly createProjectExecutionBackendError: [projectTitle: string]
  readonly getProjectExecutionDetailsBackendError: [projectTitle: string]
  readonly updateProjectExecutionBackendError: [projectTitle: string]
  readonly deleteProjectExecutionBackendError: [projectTitle: string]
  readonly listProjectExecutionsBackendError: [projectTitle: string]
  readonly syncProjectExecutionBackendError: [projectTitle: string]
  readonly getProjectLogsBackendError: [projectTitle: string]
  readonly openProjectBackendError: [projectTitle: string]
  readonly openProjectMissingCredentialsBackendError: [projectTitle: string]
  readonly updateProjectBackendError: [projectTitle: string]
  readonly uploadFileWithNameBackendError: [fileTitle: string]
  readonly getFileDetailsBackendError: [fileTitle: string]
  readonly createDatalinkBackendError: [datalinkTitle: string]
  readonly getDatalinkBackendError: [datalinkTitle: string]
  readonly deleteDatalinkBackendError: [datalinkTitle: string]
  readonly createSecretBackendError: [secretTitle: string]
  readonly createCredentialBackendError: [credentialTitle: string]
  readonly getSecretBackendError: [secretTitle: string]
  readonly updateSecretBackendError: [secretTitle: string]
  readonly createLabelBackendError: [labelName: string]
  readonly associateLabelsBackendError: [assetTitle: string]
  readonly deleteLabelBackendError: [labelName: string]
  readonly createUserGroupBackendError: [userGroupName: string]
  readonly deleteUserGroupBackendError: [userGroupName: string]
  readonly listVersionsBackendError: [versionType: string]
  readonly createCheckoutSessionBackendError: [plan: string]
  readonly getDefaultVersionBackendError: [versionType: string]
  readonly logEventBackendError: [eventType: string]

  readonly subscribeSuccessSubtitle: [plan: string]

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
  readonly teamPlanSeatsDescription: [seats: number]
  readonly tryFree: [days: number]
  readonly organizationNameSettingsInputDescription: [howLong: number]
  readonly trialDescription: [days: number]

  readonly xMinutes: [minutes: number]
  readonly xAm: [hour: string]
  readonly xPm: [hour: string]
  readonly xIsUsingTheProject: [userName: string]
  readonly xItemsCopied: [count: number]
  readonly xItemsCut: [count: number]
  readonly uploadedXFilesNotification: [count: number]
  readonly uploadingXFilesWithProgressNotification: [
    sentFiles: number,
    totalFiles: number,
    progressMb: string,
    totalMb: string,
  ]
  readonly dateXTimeX: [date: string, time: string]
  readonly projectSessionsOnX: [date: string]
  readonly monthlyXthDay: [dateOrdinal: string]
  readonly monthlyXthXDay: [weekOrdinal: string, dayOfWeek: string]
  readonly monthlyLastXDay: [dayOfWeek: string]
  readonly repeatsTimeXMonthsXDateX: [time: string, months: string, date: string]
  readonly repeatsTimeXMonthsXDayXWeekX: [time: string, months: string, day: string, week: string]
  readonly repeatsTimeXMonthsXDayXLastWeek: [time: string, months: string, day: string]
  readonly xthWeek: [weekOrdinal: string]
  readonly xExecutionsScheduledOnX: [count: number, date: string]

  readonly arbitraryFieldTooLarge: [maxSize: string]
  readonly arbitraryFieldTooSmall: [minSize: string]
  readonly uploadLargeFileStatus: [uploadedMb: string, totalMb: string]

  readonly latestVersion: [version: string, date: string]
  readonly copyInsteadOfMoving: [categoryName: string]
  readonly copyInsteadOfRestoring: [categoryName: string, destinationCategoryName: string]
  readonly copyInsteadOfRestoringDescription: [
    categoryName: string,
    destinationCategoryName: string,
  ]

  readonly plusXUsers: [count: number]
  readonly managingUserGroupX: [groupName: string]
  readonly planOverriddenToX: [planName: string]
  readonly 'manageLabelsModal.createLabelWithTitle': [labelName: string]
  readonly assetsTableBackgroundRefreshIntervalOverriddenToXMs: [ms: number]
  readonly deleteUserConfirmation: [userUsername: string, userEmail: string]
  readonly willFetchUpToXAssetsPerPage: [assetsPerPage: number]
  readonly willFetchUpToXLogEntriesPerPage: [logEntriesPerPage: number]
  readonly willUploadUpToXFileChunksAtOnce: [parallelism: number]

  readonly xDaysLeftInTrial: [daysLeft: number]
  readonly xHoursLeftInTrial: [hoursLeft: number]
  readonly yourSubscriptionExpiresAtX: [endDate: string]
  readonly commercialUseNotice: [originalTitle: string]
  readonly downgradedWarning: [daysLeft: number, hoursLeft: number]

  readonly welcomeToTeam: [organizationName: string]
  readonly invitationText: [organizationName: string]

  readonly resolveEnsoPathBackendError: [ensoPath: string]
  readonly uploadFileStartBackendError: [fileName: string]
  readonly uploadFileEndBackendError: [fileName: string]

  readonly youCanCreateXMoreApiKeys: [apiKeysLeft: number]
  readonly deleteApiKeyConfirmation: [tokenName: string]
}

// This is intentionally unused. This line throws an error if `PlaceholderOverrides` ever becomes
// out of sync with `TextId`.
// eslint-disable-next-line @typescript-eslint/no-unused-vars
type SanityCheck<T extends TextId = keyof PlaceholderOverrides> = T

/** An tuple of `string` for placeholders for each {@link TextId}. */
export interface Replacements
  extends PlaceholderOverrides,
    Record<Exclude<TextId, keyof PlaceholderOverrides>, []> {}

export const TEXTS: Readonly<Record<Language, Texts>> = {
  english: ENGLISH,
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
 * A function that gets localized text for a given key, with optional replacements.
 * @param key - The key of the text to get.
 * @param replacements - The replacements to insert into the text.
 * If the text contains placeholders like `$0`, `$1`, etc.,
 * they will be replaced with the corresponding replacement.
 */
export type DefaultGetText = <K extends TextId>(key: K, ...replacements: Replacements[K]) => string

export const defaultGetText: DefaultGetText = (key, ...replacements) => {
  return getText(TEXTS.english, key, ...replacements)
}

/** Resolves the language texts based on the user's preferred language. */
export function resolveUserLanguage(): Language {
  const locale = navigator.language
  return (
    (Object.keys(LANGUAGE_TO_LOCALE) as readonly Language[]).find(
      (language) => locale === LANGUAGE_TO_LOCALE[language],
    ) ?? 'english'
  )
}

/**
 * Gets the dictionary for a given language.
 * @param language - The language to get the dictionary for.
 * @returns The dictionary for the given language.
 */
export function getDictionary(language: Language) {
  return TEXTS[language]
}

/** Resolves the dictionary for the user's preferred language. */
export function resolveDictionary() {
  return getDictionary(resolveUserLanguage())
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
