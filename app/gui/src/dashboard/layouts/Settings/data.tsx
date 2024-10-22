/** @file Metadata for rendering each settings section. */
import type { ReactNode } from 'react'

import type { QueryClient } from '@tanstack/react-query'
import isEmail from 'validator/lib/isEmail'

import type { TextId } from 'enso-common/src/text'

import ComputerIcon from '#/assets/computer.svg'
import CreditCardIcon from '#/assets/credit_card.svg'
import KeyboardShortcutsIcon from '#/assets/keyboard_shortcuts.svg'
import LogIcon from '#/assets/log.svg'
import PeopleIcon from '#/assets/people.svg'
import PeopleSettingsIcon from '#/assets/people_settings.svg'
import SettingsIcon from '#/assets/settings.svg'
import { Button, ButtonGroup } from '#/components/AriaComponents'
import { ACTION_TO_TEXT_ID } from '#/components/MenuEntry'
import { BINDINGS } from '#/configurations/inputBindings'
import type { PaywallFeatureName } from '#/hooks/billing'
import type { ToastAndLogCallback } from '#/hooks/toastAndLogHooks'
import type { GetText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import {
  EmailAddress,
  HttpsUrl,
  isUserOnPlanWithOrganization,
  type OrganizationInfo,
  type User,
} from '#/services/Backend'
import type LocalBackend from '#/services/LocalBackend'
import type RemoteBackend from '#/services/RemoteBackend'
import { normalizePath } from '#/utilities/fileInfo'
import { unsafeEntries } from '#/utilities/object'
import ActivityLogSettingsSection from './ActivityLogSettingsSection'
import ChangePasswordForm from './ChangePasswordForm'
import DeleteUserAccountSettingsSection from './DeleteUserAccountSettingsSection'
import KeyboardShortcutsSettingsSection from './KeyboardShortcutsSettingsSection'
import MembersSettingsSection from './MembersSettingsSection'
import MembersTable from './MembersTable'
import OrganizationProfilePictureInput from './OrganizationProfilePictureInput'
import ProfilePictureInput from './ProfilePictureInput'
import { SetupTwoFaForm } from './SetupTwoFaForm'
import SettingsTabType from './TabType'
import UserGroupsSettingsSection from './UserGroupsSettingsSection'

// =========================
// === SettingsEntryType ===
// =========================

/** The tag for the {@link SettingsEntryData} discriminated union. */
export enum SettingsEntryType {
  input = 'input',
  custom = 'custom',
}

// =================
// === Constants ===
// =================

export const SETTINGS_NO_RESULTS_SECTION_DATA: SettingsSectionData = {
  nameId: 'noResultsSettingsSection',
  heading: false,
  entries: [
    {
      type: SettingsEntryType.custom,
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
    icon: SettingsIcon,
    sections: [
      {
        nameId: 'userAccountSettingsSection',
        entries: [
          {
            type: SettingsEntryType.input,
            nameId: 'userNameSettingsInput',
            getValue: (context) => context.user.name,
            setValue: async (context, newName) => {
              const oldName = context.user.name
              if (newName !== oldName) {
                await context.updateUser([{ username: newName }])
              }
            },
            validate: (name) => (/\S/.test(name) ? true : ''),
            getEditable: () => true,
          },
          {
            type: SettingsEntryType.input,
            nameId: 'userEmailSettingsInput',
            getValue: (context) => context.user.email,
            // A user's email currently cannot be changed.
            setValue: async () => {},
            validate: (email, context) =>
              isEmail(email) ? true
              : email === '' ? ''
              : context.getText('invalidEmailValidationError'),
            getEditable: () => false,
          },
        ],
      },
      {
        nameId: 'changePasswordSettingsSection',
        entries: [
          {
            type: SettingsEntryType.custom,
            aliasesId: 'changePasswordSettingsCustomEntryAliases',
            render: ChangePasswordForm,
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
        nameId: 'setup2FASettingsSection',
        entries: [
          {
            type: SettingsEntryType.custom,
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
            type: SettingsEntryType.custom,
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
            type: SettingsEntryType.custom,
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
    icon: PeopleSettingsIcon,
    organizationOnly: true,
    visible: ({ user }) => isUserOnPlanWithOrganization(user),
    sections: [
      {
        nameId: 'organizationSettingsSection',
        entries: [
          {
            type: SettingsEntryType.input,
            nameId: 'organizationNameSettingsInput',
            getValue: (context) => context.organization?.name ?? '',
            setValue: async (context, newName) => {
              const oldName = context.organization?.name ?? null
              if (oldName !== newName) {
                await context.updateOrganization([{ name: newName }])
              }
            },
            validate: (name) => (/\S/.test(name) ? true : ''),
            getEditable: (context) => context.user.isOrganizationAdmin,
          },
          {
            type: SettingsEntryType.input,
            nameId: 'organizationEmailSettingsInput',
            getValue: (context) => context.organization?.email ?? '',
            setValue: async (context, newValue) => {
              const newEmail = EmailAddress(newValue)
              const oldEmail = context.organization?.email ?? null
              if (oldEmail !== newEmail) {
                await context.updateOrganization([{ email: newEmail }])
              }
            },
            validate: (email, context) =>
              isEmail(email) ? true
              : email === '' ? ''
              : context.getText('invalidEmailValidationError'),
            getEditable: (context) => context.user.isOrganizationAdmin,
          },
          {
            type: SettingsEntryType.input,
            nameId: 'organizationWebsiteSettingsInput',
            getValue: (context) => context.organization?.website ?? '',
            setValue: async (context, newValue) => {
              const newWebsite = HttpsUrl(newValue)
              const oldWebsite = context.organization?.website ?? null
              if (oldWebsite !== newWebsite) {
                await context.updateOrganization([{ website: newWebsite }])
              }
            },
            getEditable: (context) => context.user.isOrganizationAdmin,
          },
          {
            type: SettingsEntryType.input,
            nameId: 'organizationLocationSettingsInput',
            getValue: (context) => context.organization?.address ?? '',
            setValue: async (context, newLocation) => {
              const oldLocation = context.organization?.address ?? null
              if (oldLocation !== newLocation) {
                await context.updateOrganization([{ address: newLocation }])
              }
            },
            getEditable: (context) => context.user.isOrganizationAdmin,
          },
        ],
      },
      {
        nameId: 'organizationProfilePictureSettingsSection',
        column: 2,
        entries: [
          {
            type: SettingsEntryType.custom,
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
    icon: ComputerIcon,
    visible: (context) => context.localBackend != null,
    sections: [
      {
        nameId: 'localSettingsSection',
        entries: [
          {
            type: SettingsEntryType.input,
            nameId: 'localRootPathSettingsInput',
            getValue: (context) => context.localBackend?.rootPath() ?? '',
            setValue: async (context, value) => {
              context.updateLocalRootPath(value)
              await Promise.resolve()
            },
            getEditable: () => true,
          },
          {
            type: SettingsEntryType.custom,
            aliasesId: 'localRootPathButtonSettingsCustomEntryAliases',
            render: (context) => (
              <ButtonGroup>
                {window.fileBrowserApi && (
                  <Button
                    size="small"
                    variant="outline"
                    onPress={async () => {
                      const [newDirectory] =
                        (await window.fileBrowserApi?.openFileBrowser('directory')) ?? []
                      if (newDirectory != null) {
                        context.updateLocalRootPath(normalizePath(newDirectory))
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
                  onPress={context.resetLocalRootPath}
                >
                  {context.getText('resetLocalRootDirectory')}
                </Button>
              </ButtonGroup>
            ),
          },
        ],
      },
    ],
  },
  [SettingsTabType.billingAndPlans]: {
    nameId: 'billingAndPlansSettingsTab',
    settingsTab: SettingsTabType.billingAndPlans,
    icon: CreditCardIcon,
    organizationOnly: true,
    visible: ({ user, organization }) =>
      user.isOrganizationAdmin && organization?.subscription != null,
    sections: [],
    onPress: (context) =>
      context.queryClient
        .getMutationCache()
        .build(context.queryClient, {
          mutationKey: ['billing', 'customerPortalSession'],
          mutationFn: () =>
            context.backend
              .createCustomerPortalSession()
              .then((url) => {
                if (url != null) {
                  window.open(url, '_blank')?.focus()
                }
              })
              .catch((err) => {
                context.toastAndLog('arbitraryErrorTitle', err)
                throw err
              }),
        })
        .execute({} satisfies unknown),
  },
  [SettingsTabType.members]: {
    nameId: 'membersSettingsTab',
    settingsTab: SettingsTabType.members,
    icon: PeopleIcon,
    organizationOnly: true,
    visible: ({ user }) => isUserOnPlanWithOrganization(user),
    feature: 'inviteUser',
    sections: [
      {
        nameId: 'membersSettingsSection',
        entries: [{ type: SettingsEntryType.custom, render: () => <MembersSettingsSection /> }],
      },
    ],
  },
  [SettingsTabType.userGroups]: {
    nameId: 'userGroupsSettingsTab',
    settingsTab: SettingsTabType.userGroups,
    icon: PeopleSettingsIcon,
    organizationOnly: true,
    visible: ({ user }) => isUserOnPlanWithOrganization(user),
    feature: 'userGroups',
    sections: [
      {
        nameId: 'userGroupsSettingsSection',
        columnClassName: 'h-3/5 lg:h-[unset] overflow-auto',
        entries: [
          {
            type: SettingsEntryType.custom,
            render: (context) => <UserGroupsSettingsSection backend={context.backend} />,
          },
        ],
      },
      {
        nameId: 'userGroupsUsersSettingsSection',
        column: 2,
        columnClassName: 'h-2/5 lg:h-[unset] overflow-auto',
        entries: [
          {
            type: SettingsEntryType.custom,
            render: (context) => (
              <MembersTable
                backend={context.backend}
                draggable={context.user.isOrganizationAdmin}
                populateWithSelf
              />
            ),
          },
        ],
      },
    ],
  },
  [SettingsTabType.keyboardShortcuts]: {
    nameId: 'keyboardShortcutsSettingsTab',
    settingsTab: SettingsTabType.keyboardShortcuts,
    icon: KeyboardShortcutsIcon,
    sections: [
      {
        nameId: 'keyboardShortcutsSettingsSection',
        entries: [
          {
            type: SettingsEntryType.custom,
            aliasesId: 'keyboardShortcutsSettingsCustomEntryAliases',
            getExtraAliases: (context) => {
              const rebindableBindings = unsafeEntries(BINDINGS).flatMap((kv) => {
                const [k, v] = kv
                if (v.rebindable === false) {
                  return []
                } else {
                  return ACTION_TO_TEXT_ID[k]
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
    icon: LogIcon,
    organizationOnly: true,
    visible: ({ user }) => isUserOnPlanWithOrganization(user),
    sections: [
      {
        nameId: 'activityLogSettingsSection',
        entries: [
          {
            type: SettingsEntryType.custom,
            render: (context) => <ActivityLogSettingsSection backend={context.backend} />,
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
    tabs: [SETTINGS_TAB_DATA[SettingsTabType.activityLog]],
  },
]

export const ALL_SETTINGS_TABS = SETTINGS_DATA.flatMap((section) =>
  section.tabs.map((tab) => tab.settingsTab),
)

// =======================
// === SettingsContext ===
// =======================

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
  readonly updateLocalRootPath: (rootPath: string) => void
  readonly resetLocalRootPath: () => void
  readonly toastAndLog: ToastAndLogCallback
  readonly getText: GetText
  readonly queryClient: QueryClient
}

// ==============================
// === SettingsInputEntryData ===
// ==============================

/** Metadata describing a settings entry that is an input. */
export interface SettingsInputEntryData {
  readonly type: SettingsEntryType.input
  readonly nameId: TextId & `${string}SettingsInput`
  readonly getValue: (context: SettingsContext) => string
  readonly setValue: (context: SettingsContext, value: string) => Promise<void>
  readonly validate?: (value: string, context: SettingsContext) => string | true
  readonly getEditable: (context: SettingsContext) => boolean
  readonly getVisible?: (context: SettingsContext) => boolean
}

// ===============================
// === SettingsCustomEntryData ===
// ===============================

/** Metadata describing a settings entry that needs custom rendering. */
export interface SettingsCustomEntryData {
  readonly type: SettingsEntryType.custom
  readonly aliasesId?: TextId & `${string}SettingsCustomEntryAliases`
  readonly getExtraAliases?: (context: SettingsContext) => readonly string[]
  readonly render: (context: SettingsContext) => ReactNode
  readonly getVisible?: (context: SettingsContext) => boolean
}

// =========================
// === SettingsEntryData ===
// =========================

/** A settings entry of an arbitrary type. */
export type SettingsEntryData = SettingsCustomEntryData | SettingsInputEntryData

// =======================
// === SettingsTabData ===
// =======================

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

// =======================
// === SettingsTabData ===
// =======================

/** Metadata describing a settings tab. */
export interface SettingsTabData {
  readonly nameId: TextId & `${string}SettingsTab`
  readonly settingsTab: SettingsTabType
  readonly icon: string
  readonly visible?: (context: SettingsContext) => boolean
  readonly organizationOnly?: true
  /**
   * The feature behind which this settings tab is locked. If the user cannot access the feature,
   * a paywall is shown instead of the settings tab.
   */
  readonly feature?: PaywallFeatureName
  readonly sections: readonly SettingsSectionData[]
  readonly onPress?: (context: SettingsContext) => Promise<void> | void
}

// ==============================
// === SettingsTabSectionData ===
// ==============================

/** Metadata describing a settings tab section. */
export interface SettingsTabSectionData {
  readonly nameId: TextId & `${string}SettingsTabSection`
  readonly tabs: readonly SettingsTabData[]
}

// ====================
// === SettingsData ===
// ====================

/** Metadata describing all settings. */
export type SettingsData = readonly SettingsTabSectionData[]
