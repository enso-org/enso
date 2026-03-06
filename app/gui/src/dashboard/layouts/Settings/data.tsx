/** @file Metadata for rendering each settings section. */
import { Button } from '#/components/Button'
import type { TSchema } from '#/components/Form'
import type { ComboBoxProps } from '#/components/Inputs/ComboBox'
import { actionToTextId } from '#/components/MenuEntry'
import { Text } from '#/components/Text'
import { BINDINGS } from '#/configurations/inputBindings'
import type { PaywallFeatureName } from '#/hooks/billing'
import type { ToastAndLogCallback } from '#/hooks/toastAndLogHooks'
import { setDownloadDirectory, setLocalRootDirectory } from '#/layouts/Drive/persistentState'
import { ApiKeySettingsSection } from '#/layouts/Settings/ApiKeysSettingsSection'
import { passwordWithPatternSchema } from '#/pages/authentication/schemas'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { PASSWORD_REGEX } from '#/utilities/validation'
import type { GetText } from '$/providers/text'
import type { Icon } from '@/util/iconMetadata/iconName'
import { getLocalTimeZone, now } from '@internationalized/date'
import type { QueryClient } from '@tanstack/react-query'
import type { Backend } from 'enso-common/src/services/Backend'
import {
  EmailAddress,
  HttpsUrl,
  isUserOnPlanWithMultipleSeats,
  Path,
  Plan,
  type OrganizationInfo,
  type User,
} from 'enso-common/src/services/Backend'
import type { LocalBackend } from 'enso-common/src/services/LocalBackend'
import type { RemoteBackend } from 'enso-common/src/services/RemoteBackend'
import type { TextId } from 'enso-common/src/text'
import {
  getTimeZoneFromDescription,
  getTimeZoneOffsetStringWithGMT,
  IanaTimeZone,
  tryGetDescriptionForTimeZone,
  tryGetTimeZoneFromDescription,
  WHITELISTED_TIME_ZONE_DESCRIPTIONS,
} from 'enso-common/src/utilities/data/dateTime'
import { pick, unsafeEntries } from 'enso-common/src/utilities/data/object'
import { normalizePath } from 'enso-common/src/utilities/file'
import type { HTMLInputAutoCompleteAttribute, HTMLInputTypeAttribute, ReactNode } from 'react'
import * as z from 'zod'
import ActivityLogSettingsSection from './ActivityLogSettingsSection'
import DeleteUserAccountSettingsSection from './DeleteUserAccountSettingsSection'
import KeyboardShortcutsSettingsSection from './KeyboardShortcutsSettingsSection'
import MembersSettingsSection from './MembersSettingsSection'
import OrganizationProfilePictureInput from './OrganizationProfilePictureInput'
import ProfilePictureInput from './ProfilePictureInput'
import { SetupTwoFaForm } from './SetupTwoFaForm'
import SettingsTabType from './TabType'
import { UserGroupsSettingsSection } from './UserGroupsSettingsSection'

export const SETTINGS_NO_RESULTS_SECTION_DATA: SettingsSectionData = {
  nameId: 'noResultsSettingsSection',
  heading: false,
  entries: [
    {
      type: 'custom',
      render: (context) => (
        <div className="grid max-w-[512px] justify-center">{context.getText('noResultsFound')}</div>
      ),
    },
  ],
}

export const SETTINGS_TAB_DATA: Readonly<Record<SettingsTabType, SettingsTabData>> = {
  [SettingsTabType.account]: {
    nameId: 'accountSettingsTab',
    settingsTab: SettingsTabType.account,
    icon: 'settings',
    sections: [
      {
        nameId: 'userAccountSettingsSection',
        entries: [
          settingsFormEntryData({
            type: 'form',
            schema: z.object({
              name: z.string().min(1),
              email: z.string().email().or(z.literal('')),
              timeZone: z.string().optional(),
            }),
            getValue: (context) => ({
              ...pick(context.user, 'name', 'email'),
              timeZone: tryGetDescriptionForTimeZone(
                context.preferredTimeZone,
                IanaTimeZone(getLocalTimeZone()),
              ),
            }),
            onSubmit: async (context, { name, timeZone }) => {
              const newTimeZone = timeZone != null ? tryGetTimeZoneFromDescription(timeZone) : null
              if (newTimeZone != null) {
                context.setPreferredTimeZone(newTimeZone)
              }
              if (name !== context.user.name) {
                await context.updateUser([{ username: name }])
              }
            },
            inputs: [
              { nameId: 'userNameSettingsInput', name: 'name' },
              { nameId: 'userEmailSettingsInput', name: 'email', editable: false },
              {
                nameId: 'userTimeZoneSettingsInput',
                descriptionId: 'userTimeZoneSettingsInputDescription',
                name: 'timeZone',
                type: 'comboBox',
                comboBoxProps: () => ({
                  items: WHITELISTED_TIME_ZONE_DESCRIPTIONS,
                  addonStart: (description: string | null) => {
                    const timeZone =
                      description != null ? tryGetTimeZoneFromDescription(description) : null
                    return (
                      <Text className="w-20">
                        {getTimeZoneOffsetStringWithGMT(now(timeZone ?? getLocalTimeZone()))}
                      </Text>
                    )
                  },
                  toTextValue: (timeZone: string) => timeZone,
                  children: (description: string) => {
                    const otherTimeZone = getTimeZoneFromDescription(description)
                    const timezoneOffsetString = getTimeZoneOffsetStringWithGMT(now(otherTimeZone))
                    return `${timezoneOffsetString} ${description}`
                  },
                }),
                hidden: (context) =>
                  context.user.plan === Plan.free || context.user.plan === Plan.solo,
              },
            ],
          }),
        ],
      },
      {
        nameId: 'changePasswordSettingsSection',
        entries: [
          settingsFormEntryData({
            type: 'form',
            schema: ({ getText }) =>
              z
                .object({
                  username: z.string().email(getText('invalidEmailValidationError')),
                  // We don't want to validate the current password.
                  currentPassword: z.string(),
                  newPassword: passwordWithPatternSchema(getText),
                  confirmNewPassword: z.string(),
                })
                .superRefine((object, context) => {
                  if (
                    PASSWORD_REGEX.test(object.newPassword) &&
                    object.newPassword !== object.confirmNewPassword
                  ) {
                    context.addIssue({
                      path: ['confirmNewPassword'],
                      code: 'custom',
                      message: getText('passwordMismatchError'),
                    })
                  }
                }),
            getValue: ({ user }) => ({
              username: user.email,
              currentPassword: '',
              newPassword: '',
              confirmNewPassword: '',
            }),
            onSubmit: async ({ changePassword }, { currentPassword, newPassword }) => {
              await changePassword(currentPassword, newPassword)
            },
            inputs: [
              {
                nameId: 'userNameSettingsInput',
                name: 'username',
                autoComplete: 'username',
                editable: false,
                hidden: true,
              },
              {
                nameId: 'userCurrentPasswordSettingsInput',
                name: 'currentPassword',
                autoComplete: 'current-assword',
                type: 'password',
              },
              {
                nameId: 'userNewPasswordSettingsInput',
                name: 'newPassword',
                autoComplete: 'new-password',
                descriptionId: 'passwordValidationMessage',
                type: 'password',
              },
              {
                nameId: 'userConfirmNewPasswordSettingsInput',
                name: 'confirmNewPassword',
                autoComplete: 'new-password',
                type: 'password',
              },
            ],
            getVisible: (context) => {
              // The shape of the JWT payload is statically known.
              // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
              const username: string | null =
                // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-non-null-assertion
                JSON.parse(atob(context.accessToken.split('.')[1]!)).username
              return username != null ? !/^Github_|^Google_/.test(username) : false
            },
          }),
        ],
      },
      {
        nameId: 'setup2FASettingsSection',
        entries: [
          {
            type: 'custom',
            render: SetupTwoFaForm,
            getVisible: (context) => {
              // The shape of the JWT payload is statically known.
              // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
              const username: string | null =
                // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-non-null-assertion
                JSON.parse(atob(context.accessToken.split('.')[1]!)).username
              return username != null ? !/^Github_|^Google_/.test(username) : false
            },
          },
        ],
      },
      {
        nameId: 'deleteUserAccountSettingsSection',
        heading: false,
        entries: [
          {
            type: 'custom',
            aliasesId: 'deleteUserAccountSettingsCustomEntryAliases',
            render: () => <DeleteUserAccountSettingsSection />,
          },
        ],
      },
      {
        nameId: 'profilePictureSettingsSection',
        column: 2,
        entries: [
          {
            type: 'custom',
            aliasesId: 'profilePictureSettingsCustomEntryAliases',
            render: (context) => <ProfilePictureInput backend={context.backend} />,
          },
        ],
      },
    ],
  },
  [SettingsTabType.organization]: {
    nameId: 'organizationSettingsTab',
    settingsTab: SettingsTabType.organization,
    icon: 'people_settings',
    organizationOnly: true,
    visible: ({ user }) => isUserOnPlanWithMultipleSeats(user),
    sections: [
      {
        nameId: 'organizationSettingsSection',
        entries: [
          settingsFormEntryData({
            type: 'form',
            schema: z.object({
              name: z.string().regex(/^.*\S.*$|^$/),
              email: z.string().email().or(z.literal('')),
              website: z.string(),
              address: z.string(),
            }),
            getValue: (context) => {
              const { name, email, website, address } = context.organization ?? {}
              return {
                name: name ?? '',
                email: String(email ?? ''),
                website: String(website ?? ''),
                address: address ?? '',
              }
            },
            onSubmit: async (context, { name, email, website, address }) => {
              await context.updateOrganization([
                {
                  name,
                  email: EmailAddress(email),
                  website: HttpsUrl(website),
                  address,
                },
              ])
            },
            inputs: [
              {
                nameId: 'organizationNameSettingsInput',
                name: 'name',
                editable: (context) => context.user.isOrganizationAdmin,
              },
              {
                nameId: 'organizationEmailSettingsInput',
                name: 'email',
                editable: (context) => context.user.isOrganizationAdmin,
              },
              {
                nameId: 'organizationWebsiteSettingsInput',
                name: 'website',
                editable: (context) => context.user.isOrganizationAdmin,
              },
              {
                nameId: 'organizationLocationSettingsInput',
                name: 'address',
                editable: (context) => context.user.isOrganizationAdmin,
              },
            ],
          }),
        ],
      },
      {
        nameId: 'organizationProfilePictureSettingsSection',
        column: 2,
        entries: [
          {
            type: 'custom',
            aliasesId: 'organizationProfilePictureSettingsCustomEntryAliases',
            render: (context) => <OrganizationProfilePictureInput backend={context.backend} />,
          },
        ],
      },
    ],
  },
  [SettingsTabType.local]: {
    nameId: 'localSettingsTab',
    settingsTab: SettingsTabType.local,
    icon: 'system',
    visible: ({ localBackend }) => localBackend != null,
    sections: [
      {
        nameId: 'localSettingsSection',
        entries: [
          settingsFormEntryData({
            type: 'form',
            schema: z.object({
              localRootDirectory: z.string(),
            }),
            getValue: ({ localRootDirectory }) => ({
              localRootDirectory: String(localRootDirectory ?? ''),
            }),
            onSubmit: (_, { localRootDirectory }) => {
              setLocalRootDirectory(Path(localRootDirectory))
            },
            inputs: [{ nameId: 'localRootPathSettingsInput', name: 'localRootDirectory' }],
          }),
          {
            type: 'custom',
            aliasesId: 'localRootPathButtonSettingsCustomEntryAliases',
            render: (context) => (
              <Button.Group className="grow-0">
                {window.api && (
                  <Button
                    size="small"
                    variant="outline"
                    onPress={async () => {
                      const [newDirectory] =
                        (await window.api?.fileBrowser.openFileBrowser('directory')) ?? []
                      if (newDirectory != null) {
                        setLocalRootDirectory(Path(normalizePath(newDirectory)))
                      }
                    }}
                  >
                    {context.getText('browseForNewLocalRootDirectory')}
                  </Button>
                )}
                <Button
                  size="small"
                  variant="outline"
                  className="self-start"
                  onPress={() => {
                    setLocalRootDirectory(null)
                  }}
                >
                  {context.getText('resetLocalRootDirectory')}
                </Button>
              </Button.Group>
            ),
          },
          settingsFormEntryData({
            type: 'form',
            schema: z.object({
              downloadDirectory: z.string(),
            }),
            getValue: ({ downloadDirectory }) => ({
              downloadDirectory: String(downloadDirectory ?? ''),
            }),
            onSubmit: (_, { downloadDirectory }) => {
              setDownloadDirectory(Path(downloadDirectory))
            },
            inputs: [{ nameId: 'downloadDirectorySettingsInput', name: 'downloadDirectory' }],
          }),
          {
            type: 'custom',
            aliasesId: 'downloadDirectoryButtonSettingsCustomEntryAliases',
            render: (context) => (
              <Button.Group className="grow-0">
                {window.api && (
                  <Button
                    size="small"
                    variant="outline"
                    onPress={async () => {
                      const [newDirectory] =
                        (await window.api?.fileBrowser.openFileBrowser('directory')) ?? []
                      if (newDirectory != null) {
                        setDownloadDirectory(Path(normalizePath(newDirectory)))
                      }
                    }}
                  >
                    {context.getText('browseForNewDownloadDirectory')}
                  </Button>
                )}
                <Button
                  size="small"
                  variant="outline"
                  className="self-start"
                  onPress={() => {
                    setDownloadDirectory(null)
                  }}
                >
                  {context.getText('resetDownloadDirectory')}
                </Button>
              </Button.Group>
            ),
          },
        ],
      },
    ],
  },
  [SettingsTabType.billingAndPlans]: {
    nameId: 'billingAndPlansSettingsTab',
    settingsTab: SettingsTabType.billingAndPlans,
    icon: 'credit_card',
    organizationOnly: true,
    visible: ({ user, organization }) =>
      user.isOrganizationAdmin && organization?.subscription != null,
    sections: [
      {
        nameId: 'billingAndPlansSettingsSection',
        entries: [
          {
            type: 'custom',
            aliasesId: 'billingAndPlansSettingsCustomEntryAliases',
            render: (context) => {
              // This is a React component, so we can use hooks.
              // eslint-disable-next-line react-hooks/rules-of-hooks
              const openCustomerPortalSession = useMutationCallback({
                mutationKey: ['billing', 'customerPortalSession'],
                mutationFn: () =>
                  context.backend.createCustomerPortalSession().then(
                    (url) => {
                      if (url != null) {
                        window.open(url, '_blank')?.focus()
                      }
                    },
                    (error) => {
                      context.toastAndLog('arbitraryErrorTitle', error)
                      throw error
                    },
                  ),
              })

              return (
                <Button.Group className="grow-0">
                  <Button
                    size="small"
                    variant="outline"
                    className="self-start"
                    onPress={() => openCustomerPortalSession()}
                  >
                    {context.getText('openBillingPage')}
                  </Button>
                </Button.Group>
              )
            },
          },
        ],
      },
    ],
  },
  [SettingsTabType.members]: {
    nameId: 'membersSettingsTab',
    settingsTab: SettingsTabType.members,
    icon: 'people',
    organizationOnly: true,
    visible: ({ user }) => isUserOnPlanWithMultipleSeats(user) && user.isOrganizationAdmin,
    feature: 'inviteUser',
    sections: [
      {
        nameId: 'membersSettingsSection',
        columnClassName: 'h-full *:flex-1 *:min-h-0',
        entries: [{ type: 'custom', render: MembersSettingsSection }],
      },
    ],
  },
  [SettingsTabType.userGroups]: {
    nameId: 'userGroupsSettingsTab',
    settingsTab: SettingsTabType.userGroups,
    icon: 'people_settings',
    organizationOnly: true,
    visible: ({ user }) => isUserOnPlanWithMultipleSeats(user) && user.isOrganizationAdmin,
    feature: 'userGroups',
    sections: [
      {
        nameId: 'userGroupsSettingsSection',
        columnClassName: 'h-full *:flex-1 *:min-h-0 max-w-[unset]',
        entries: [{ type: 'custom', render: UserGroupsSettingsSection }],
      },
    ],
  },
  [SettingsTabType.keyboardShortcuts]: {
    nameId: 'keyboardShortcutsSettingsTab',
    settingsTab: SettingsTabType.keyboardShortcuts,
    icon: 'keyboard_shortcuts',
    sections: [
      {
        nameId: 'keyboardShortcutsSettingsSection',
        columnClassName: 'h-full *:flex-1 *:min-h-0 max-w-[unset]',
        entries: [
          {
            type: 'custom',
            aliasesId: 'keyboardShortcutsSettingsCustomEntryAliases',
            getExtraAliases: (context) => {
              const rebindableBindings = unsafeEntries(BINDINGS).flatMap((kv) => {
                const [k, v] = kv
                if (v.rebindable === false) {
                  return []
                } else {
                  return [actionToTextId(k)]
                }
              })
              return rebindableBindings.map((binding) => context.getText(binding))
            },
            render: KeyboardShortcutsSettingsSection,
          },
        ],
      },
    ],
  },
  [SettingsTabType.activityLog]: {
    nameId: 'activityLogSettingsTab',
    settingsTab: SettingsTabType.activityLog,
    icon: 'log',
    organizationOnly: true,
    visible: ({ user }) => isUserOnPlanWithMultipleSeats(user),
    sections: [
      {
        nameId: 'activityLogSettingsSection',
        columnClassName: 'h-full *:flex-1 *:min-h-0 max-w-[unset]',
        entries: [
          {
            type: 'custom',
            render: (context) => <ActivityLogSettingsSection backend={context.backend} />,
          },
        ],
      },
    ],
  },
  [SettingsTabType.apiKeys]: {
    nameId: 'apiKeysSettingsTab',
    settingsTab: SettingsTabType.apiKeys,
    icon: 'key',
    sections: [
      {
        nameId: 'apiKeysSettingsSection',
        columnClassName: 'h-full *:flex-1 *:min-h-0 max-w-[unset]',
        entries: [
          {
            type: 'custom',
            aliasesId: 'apiKeysSettingsCustomEntryAliases',
            render: () => <ApiKeySettingsSection />,
          },
        ],
      },
    ],
  },
}

export const SETTINGS_DATA: SettingsData = [
  {
    nameId: 'generalSettingsTabSection',
    tabs: [
      SETTINGS_TAB_DATA[SettingsTabType.account],
      SETTINGS_TAB_DATA[SettingsTabType.organization],
      SETTINGS_TAB_DATA[SettingsTabType.local],
    ],
  },
  {
    nameId: 'accessSettingsTabSection',
    tabs: [
      SETTINGS_TAB_DATA[SettingsTabType.billingAndPlans],
      SETTINGS_TAB_DATA[SettingsTabType.members],
      SETTINGS_TAB_DATA[SettingsTabType.userGroups],
    ],
  },
  {
    nameId: 'lookAndFeelSettingsTabSection',
    tabs: [SETTINGS_TAB_DATA[SettingsTabType.keyboardShortcuts]],
  },
  {
    nameId: 'securitySettingsTabSection',
    tabs: [
      SETTINGS_TAB_DATA[SettingsTabType.activityLog],
      SETTINGS_TAB_DATA[SettingsTabType.apiKeys],
    ],
  },
]

export const ALL_SETTINGS_TABS = SETTINGS_DATA.flatMap((section) =>
  section.tabs.map((tab) => tab.settingsTab),
)

/** Metadata describing inputs passed to every settings entry. */
export interface SettingsContext {
  readonly accessToken: string
  readonly user: User
  readonly backend: RemoteBackend
  readonly localBackend: LocalBackend | null
  readonly organization: OrganizationInfo | null
  readonly updateUser: (variables: Parameters<Backend['updateUser']>) => Promise<void>
  readonly updateOrganization: (
    variables: Parameters<Backend['updateOrganization']>,
  ) => Promise<OrganizationInfo | null | undefined>
  readonly localRootDirectory: Path | null
  readonly downloadDirectory: Path | null
  readonly toastAndLog: ToastAndLogCallback
  readonly getText: GetText
  readonly queryClient: QueryClient
  readonly isMatch: (name: string) => boolean
  readonly changePassword: (oldPassword: string, newPassword: string) => Promise<boolean>
  readonly preferredTimeZone: string | undefined
  readonly setPreferredTimeZone: (preferredTimeZone: string | undefined) => void
}

/**
 * Possible values for the `type` property of {@link SettingsInputData}.
 *
 * TODO: Add support for other types.
 */
export type SettingsInputType =
  | Extract<HTMLInputTypeAttribute, 'email' | 'password' | 'text'>
  | 'comboBox'

/** Either `T`, or a function that returns `T` given a `SettingsContext`. */
type ToValue<T> = T | ((context: SettingsContext) => T)

/** Metadata describing an input in a {@link SettingsFormEntryData}. */
interface SettingsInputDataBase<T> {
  readonly nameId: TextId & `${string}SettingsInput`
  readonly name: string & keyof T
  readonly autoComplete?: HTMLInputAutoCompleteAttribute
  /** Defaults to `false`. */
  readonly hidden?: ToValue<boolean>
  /** Defaults to `true`. */
  readonly editable?: ToValue<boolean>
  readonly descriptionId?: TextId
}

/** Metadata describing a native input in a {@link SettingsFormEntryData}. */
interface SettingsNativeInputData<T> extends SettingsInputDataBase<T> {
  readonly type?: Extract<HTMLInputTypeAttribute, SettingsInputType>
}

/** The relevant `ComboBox` props for a {@link SettingsComboBoxInputData}. */
type ComboBoxPartialProps = Partial<ComboBoxProps<TSchema, string>> &
  Pick<ComboBoxProps<TSchema, string>, 'items'>

/** Metadata describing a combo box input in a {@link SettingsFormEntryData}. */
interface SettingsComboBoxInputData<T> extends SettingsInputDataBase<T> {
  readonly comboBoxProps: ToValue<ComboBoxPartialProps>
  readonly type: 'comboBox'
}

/** Metadata describing an input in a {@link SettingsFormEntryData}. */
export type SettingsInputData<T> = SettingsComboBoxInputData<T> | SettingsNativeInputData<T>

/** Metadata describing a settings entry that is a form. */
export interface SettingsFormEntryData<T> {
  readonly type: 'form'
  readonly schema: z.ZodType<T> | ((context: SettingsContext) => z.ZodType<T>)
  readonly getValue: (context: SettingsContext) => NoInfer<T>
  readonly onSubmit: (context: SettingsContext, value: NoInfer<T>) => Promise<void> | void
  readonly inputs: readonly SettingsInputData<NoInfer<T>>[]
  readonly getVisible?: (context: SettingsContext) => boolean
}

/** A type-safe function to define a {@link SettingsFormEntryData}. */
function settingsFormEntryData<T>(data: SettingsFormEntryData<T>) {
  return data
}

/** Metadata describing a settings entry that needs custom rendering. */
export interface SettingsCustomEntryData {
  readonly type: 'custom'
  readonly aliasesId?: TextId & `${string}SettingsCustomEntryAliases`
  readonly getExtraAliases?: (context: SettingsContext) => readonly string[]
  readonly render: (context: SettingsContext) => ReactNode
  readonly getVisible?: (context: SettingsContext) => boolean
}

/** A settings entry of an arbitrary type. */
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export type SettingsEntryData = SettingsCustomEntryData | SettingsFormEntryData<any>

/** Metadata describing a settings section. */
export interface SettingsSectionData {
  readonly nameId: TextId & `${string}SettingsSection`
  /** The first column is column 1, not column 0. */
  readonly column?: number
  readonly heading?: false
  readonly focusArea?: false
  readonly columnClassName?: string
  readonly aliases?: TextId[]
  readonly entries: readonly SettingsEntryData[]
}

/** Metadata describing a settings tab. */
export interface SettingsTabData {
  readonly nameId: TextId & `${string}SettingsTab`
  readonly settingsTab: SettingsTabType
  readonly icon: Icon
  readonly visible?: (context: SettingsContext) => boolean
  readonly organizationOnly?: true
  /**
   * The feature behind which this settings tab is locked. If the user cannot access the feature,
   * a paywall is shown instead of the settings tab.
   */
  readonly feature?: PaywallFeatureName
  readonly sections: readonly SettingsSectionData[]
}

/** Metadata describing a settings tab section. */
export interface SettingsTabSectionData {
  readonly nameId: TextId & `${string}SettingsTabSection`
  readonly tabs: readonly SettingsTabData[]
}

/** Metadata describing all settings. */
export type SettingsData = readonly SettingsTabSectionData[]
