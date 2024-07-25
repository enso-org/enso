/** @file Metadata for rendering each settings section. */
import * as React from 'react'

import type * as reactQuery from '@tanstack/react-query'
import isEmail from 'validator/lib/isEmail'

import type * as text from 'enso-common/src/text'

import ComputerIcon from '#/assets/computer.svg'
import CreditCardIcon from '#/assets/credit_card.svg'
import KeyboardShortcutsIcon from '#/assets/keyboard_shortcuts.svg'
import LogIcon from '#/assets/log.svg'
import PeopleSettingsIcon from '#/assets/people_settings.svg'
import PeopleIcon from '#/assets/people.svg'
import SettingsIcon from '#/assets/settings.svg'

import * as inputBindings from '#/configurations/inputBindings'

import type * as billing from '#/hooks/billing'
import type * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import type * as textProvider from '#/providers/TextProvider'

import ActivityLogSettingsSection from '#/layouts/Settings/ActivityLogSettingsSection'
import ChangePasswordForm from '#/layouts/Settings/ChangePasswordForm'
import DeleteUserAccountSettingsSection from '#/layouts/Settings/DeleteUserAccountSettingsSection'
import KeyboardShortcutsSettingsSection from '#/layouts/Settings/KeyboardShortcutsSettingsSection'
import MembersSettingsSection from '#/layouts/Settings/MembersSettingsSection'
import MembersTable from '#/layouts/Settings/MembersTable'
import OrganizationProfilePictureInput from '#/layouts/Settings/OrganizationProfilePictureInput'
import ProfilePictureInput from '#/layouts/Settings/ProfilePictureInput'
import UserGroupsSettingsSection from '#/layouts/Settings/UserGroupsSettingsSection'

import * as menuEntry from '#/components/MenuEntry'

import * as backend from '#/services/Backend'
import type Backend from '#/services/Backend'
import type LocalBackend from '#/services/LocalBackend'
import type RemoteBackend from '#/services/RemoteBackend'

import * as object from '#/utilities/object'
import { includesPredicate } from 'enso-common/src/utilities/data/array'

// =================
// === Constants ===
// =================

/** A sub-page of the settings page. */
const SETTINGS_TAB_TYPES = [
  'account',
  'organization',
  'local',
  // 'features',
  // 'notifications',
  'billing-and-plans',
  'members',
  'user-groups',
  // 'appearance',
  'keyboard-shortcuts',
  // 'data-co-pilot',
  // 'feature-preview',
  'activity-log',
] as const

export type SettingsTabType = (typeof SETTINGS_TAB_TYPES)[number]

export const isSettingsTabType = includesPredicate(SETTINGS_TAB_TYPES)

export const SETTINGS_NO_RESULTS_SECTION_DATA: SettingsSectionData = {
  nameId: 'noResultsSettingsSection',
  heading: false,
  entries: [
    {
      type: 'custom',
      render: context => (
        <div className="grid max-w-[512px] justify-center">{context.getText('noResultsFound')}</div>
      ),
    },
  ],
}

export const SETTINGS_TAB_DATA: Readonly<Record<SettingsTabType, SettingsTabData>> = {
  account: {
    nameId: 'accountSettingsTab',
    settingsTab: 'account',
    icon: SettingsIcon,
    sections: [
      {
        nameId: 'userAccountSettingsSection',
        entries: [
          {
            type: 'input',
            nameId: 'userNameSettingsInput',
            getValue: context => context.user.name,
            setValue: async (context, newName) => {
              const oldName = context.user.name
              if (newName !== oldName) {
                await context.updateUser([{ username: newName }])
              }
            },
            validate: name => (/\S/.test(name) ? true : ''),
            getEditable: () => true,
          },
          {
            type: 'input',
            nameId: 'userEmailSettingsInput',
            getValue: context => context.user.email,
            // A user's email currently cannot be changed.
            setValue: async () => {},
            validate: (email, context) =>
              isEmail(email)
                ? true
                : email === ''
                  ? ''
                  : context.getText('invalidEmailValidationError'),
            getEditable: () => false,
          },
        ],
      },
      {
        nameId: 'changePasswordSettingsSection',
        entries: [
          {
            type: 'custom',
            aliasesId: 'changePasswordSettingsCustomEntryAliases',
            render: ChangePasswordForm,
            getVisible: context => {
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
            render: context => <ProfilePictureInput backend={context.backend} />,
          },
        ],
      },
    ],
  },
  organization: {
    nameId: 'organizationSettingsTab',
    settingsTab: 'organization',
    icon: PeopleSettingsIcon,
    organizationOnly: true,
    sections: [
      {
        nameId: 'organizationSettingsSection',
        entries: [
          {
            type: 'input',
            nameId: 'organizationNameSettingsInput',
            getValue: context => context.organization?.name ?? '',
            setValue: async (context, newName) => {
              const oldName = context.organization?.name ?? null
              if (oldName !== newName) {
                await context.updateOrganization([{ name: newName }])
              }
            },
            validate: name => (/\S/.test(name) ? true : ''),
            getEditable: () => true,
          },
          {
            type: 'input',
            nameId: 'organizationEmailSettingsInput',
            getValue: context => context.organization?.email ?? '',
            setValue: async (context, newValue) => {
              const newEmail = backend.EmailAddress(newValue)
              const oldEmail = context.organization?.email ?? null
              if (oldEmail !== newEmail) {
                await context.updateOrganization([{ email: newEmail }])
              }
            },
            validate: (email, context) =>
              isEmail(email)
                ? true
                : email === ''
                  ? ''
                  : context.getText('invalidEmailValidationError'),
            getEditable: () => true,
          },
          {
            type: 'input',
            nameId: 'organizationWebsiteSettingsInput',
            getValue: context => context.organization?.website ?? '',
            setValue: async (context, newValue) => {
              const newWebsite = backend.HttpsUrl(newValue)
              const oldWebsite = context.organization?.website ?? null
              if (oldWebsite !== newWebsite) {
                await context.updateOrganization([{ website: newWebsite }])
              }
            },
            getEditable: () => true,
          },
          {
            type: 'input',
            nameId: 'organizationLocationSettingsInput',
            getValue: context => context.organization?.address ?? '',
            setValue: async (context, newLocation) => {
              const oldLocation = context.organization?.address ?? null
              if (oldLocation !== newLocation) {
                await context.updateOrganization([{ address: newLocation }])
              }
            },
            getEditable: () => true,
          },
        ],
      },
      {
        nameId: 'organizationProfilePictureSettingsSection',
        column: 2,
        entries: [
          {
            type: 'custom',
            aliasesId: 'organizationProfilePictureSettingsCustomEntryAliases',
            render: context => <OrganizationProfilePictureInput backend={context.backend} />,
          },
        ],
      },
    ],
  },
  local: {
    nameId: 'localSettingsTab',
    settingsTab: 'organization',
    icon: ComputerIcon,
    visible: context => context.localBackend != null,
    sections: [
      {
        nameId: 'localSettingsSection',
        entries: [
          {
            type: 'input',
            nameId: 'localRootPathSettingsInput',
            getValue: context => context.localBackend?.rootPath ?? '',
            setValue: (context, value) => context.updateLocalRootPath(value),
            getEditable: () => true,
          },
        ],
      },
    ],
  },
  'billing-and-plans': {
    nameId: 'billingAndPlansSettingsTab',
    settingsTab: 'billing-and-plans',
    icon: CreditCardIcon,
    organizationOnly: true,
    visible: context => context.organization?.subscription != null,
    sections: [],
    onPress: context =>
      context.queryClient
        .getMutationCache()
        .build(context.queryClient, {
          mutationKey: ['billing', 'customerPortalSession'],
          mutationFn: () =>
            context.backend
              .createCustomerPortalSession()
              .then(url => {
                if (url != null) {
                  window.open(url, '_blank')?.focus()
                }
              })
              .catch(err => {
                context.toastAndLog('arbitraryErrorTitle', err)
                throw err
              }),
        })
        .execute({} satisfies unknown),
  },
  members: {
    nameId: 'membersSettingsTab',
    settingsTab: 'members',
    icon: PeopleIcon,
    organizationOnly: true,
    feature: 'inviteUser',
    sections: [
      {
        nameId: 'membersSettingsSection',
        entries: [{ type: 'custom', render: () => <MembersSettingsSection /> }],
      },
    ],
  },
  'user-groups': {
    nameId: 'userGroupsSettingsTab',
    settingsTab: 'user-groups',
    icon: PeopleSettingsIcon,
    organizationOnly: true,
    feature: 'userGroups',
    sections: [
      {
        nameId: 'userGroupsSettingsSection',
        columnClassName: 'h-3/5 lg:h-[unset] overflow-auto',
        entries: [
          {
            type: 'custom',
            render: context => <UserGroupsSettingsSection backend={context.backend} />,
          },
        ],
      },
      {
        nameId: 'userGroupsUsersSettingsSection',
        column: 2,
        columnClassName: 'h-2/5 lg:h-[unset] overflow-auto',
        entries: [
          {
            type: 'custom',
            render: context => (
              <MembersTable backend={context.backend} draggable populateWithSelf />
            ),
          },
        ],
      },
    ],
  },
  'keyboard-shortcuts': {
    nameId: 'keyboardShortcutsSettingsTab',
    settingsTab: 'keyboard-shortcuts',
    icon: KeyboardShortcutsIcon,
    sections: [
      {
        nameId: 'keyboardShortcutsSettingsSection',
        entries: [
          {
            type: 'custom',
            aliasesId: 'keyboardShortcutsSettingsCustomEntryAliases',
            getExtraAliases: context => {
              const rebindableBindings = object
                .unsafeEntries(inputBindings.BINDINGS)
                .flatMap(kv => {
                  const [k, v] = kv
                  if (v.rebindable === false) {
                    return []
                  } else {
                    return menuEntry.ACTION_TO_TEXT_ID[k]
                  }
                })
              return rebindableBindings.map(binding => context.getText(binding))
            },
            render: KeyboardShortcutsSettingsSection,
          },
        ],
      },
    ],
  },
  'activity-log': {
    nameId: 'activityLogSettingsTab',
    settingsTab: 'activity-log',
    icon: LogIcon,
    organizationOnly: true,
    sections: [
      {
        nameId: 'activityLogSettingsSection',
        entries: [
          {
            type: 'custom',
            render: context => <ActivityLogSettingsSection backend={context.backend} />,
          },
        ],
      },
    ],
  },
}

export const SETTINGS_DATA: SettingsData = [
  {
    nameId: 'generalSettingsTabSection',
    tabs: [SETTINGS_TAB_DATA['account'], SETTINGS_TAB_DATA['organization']],
  },
  {
    nameId: 'accessSettingsTabSection',
    tabs: [
      SETTINGS_TAB_DATA['billing-and-plans'],
      SETTINGS_TAB_DATA['members'],
      SETTINGS_TAB_DATA['user-groups'],
    ],
  },
  {
    nameId: 'lookAndFeelSettingsTabSection',
    tabs: [SETTINGS_TAB_DATA['keyboard-shortcuts']],
  },
  {
    nameId: 'securitySettingsTabSection',
    tabs: [SETTINGS_TAB_DATA['activity-log']],
  },
]

export const ALL_SETTINGS_TABS = SETTINGS_DATA.flatMap(section =>
  section.tabs.map(tab => tab.settingsTab)
)

// =======================
// === SettingsContext ===
// =======================

/** Metadata describing inputs passed to every settings entry. */
export interface SettingsContext {
  readonly accessToken: string
  readonly user: backend.User
  readonly backend: RemoteBackend
  readonly localBackend: LocalBackend | null
  readonly organization: backend.OrganizationInfo | null
  readonly updateUser: (variables: Parameters<Backend['updateUser']>) => Promise<void>
  readonly updateOrganization: (
    variables: Parameters<Backend['updateOrganization']>
  ) => Promise<backend.OrganizationInfo | null | undefined>
  readonly updateLocalRootPath: (rootPath: string) => Promise<void>
  readonly toastAndLog: toastAndLogHooks.ToastAndLogCallback
  readonly getText: textProvider.GetText
  readonly queryClient: reactQuery.QueryClient
}

// ==============================
// === SettingsInputEntryData ===
// ==============================

/** Metadata describing a settings entry that is an input. */
export interface SettingsInputEntryData {
  readonly type: 'input'
  readonly nameId: text.TextId & `${string}SettingsInput`
  readonly getValue: (context: SettingsContext) => string
  readonly setValue: (context: SettingsContext, value: string) => Promise<void>
  readonly validate?: (value: string, context: SettingsContext) => string | true
  readonly getEditable: (context: SettingsContext) => boolean
}

// ===============================
// === SettingsCustomEntryData ===
// ===============================

/** Metadata describing a settings entry that needs custom rendering. */
export interface SettingsCustomEntryData {
  readonly type: 'custom'
  readonly aliasesId?: text.TextId & `${string}SettingsCustomEntryAliases`
  readonly getExtraAliases?: (context: SettingsContext) => readonly string[]
  readonly render: (context: SettingsContext) => React.ReactNode
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
  readonly nameId: text.TextId & `${string}SettingsSection`
  /** The first column is column 1, not column 0. */
  readonly column?: number
  readonly heading?: false
  readonly focusArea?: false
  readonly columnClassName?: string
  readonly aliases?: text.TextId[]
  readonly entries: readonly SettingsEntryData[]
}

// =======================
// === SettingsTabData ===
// =======================

/** Metadata describing a settings tab. */
export interface SettingsTabData {
  readonly nameId: text.TextId & `${string}SettingsTab`
  readonly settingsTab: SettingsTabType
  readonly icon: string
  readonly visible?: (context: SettingsContext) => boolean
  readonly organizationOnly?: true
  /** The feature behind which this settings tab is locked. If the user cannot access the feature,
   * a paywall is shown instead of the settings tab. */
  readonly feature?: billing.PaywallFeatureName
  readonly sections: readonly SettingsSectionData[]
  readonly onPress?: (context: SettingsContext) => Promise<void> | void
}

// ==============================
// === SettingsTabSectionData ===
// ==============================

/** Metadata describing a settings tab section. */
export interface SettingsTabSectionData {
  readonly nameId: text.TextId & `${string}SettingsTabSection`
  readonly tabs: readonly SettingsTabData[]
}

// ====================
// === SettingsData ===
// ====================

/** Metadata describing all settings. */
export type SettingsData = readonly SettingsTabSectionData[]
