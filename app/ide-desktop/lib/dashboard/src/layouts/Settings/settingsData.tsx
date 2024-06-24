/** @file Metadata for rendering each settings section. */
import * as React from 'react'

import type * as reactQuery from '@tanstack/react-query'
import isEmail from 'validator/lib/isEmail'

import KeyboardShortcutsIcon from 'enso-assets/keyboard_shortcuts.svg'
import LogIcon from 'enso-assets/log.svg'
import PeopleSettingsIcon from 'enso-assets/people_settings.svg'
import PeopleIcon from 'enso-assets/people.svg'
import SettingsIcon from 'enso-assets/settings.svg'

import type * as text from '#/text'

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
import SettingsTabType from '#/layouts/Settings/SettingsTabType'
import UserGroupsSettingsSection from '#/layouts/Settings/UserGroupsSettingsSection'

import * as menuEntry from '#/components/MenuEntry'

import * as backend from '#/services/Backend'
import type Backend from '#/services/Backend'

import * as object from '#/utilities/object'

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
            getValue: context => context.user.name,
            setValue: async (context, newName, reset) => {
              const oldName = context.user.name
              if (newName !== oldName) {
                await context.updateUser([{ username: newName }]).catch(reset)
              }
            },
            validate: name => (/\S/.test(name) ? true : ''),
            getEditable: () => true,
          },
          {
            type: SettingsEntryType.input,
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
            type: SettingsEntryType.custom,
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
            render: context => context.backend && <ProfilePictureInput backend={context.backend} />,
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
    sections: [
      {
        nameId: 'organizationSettingsSection',
        entries: [
          {
            type: SettingsEntryType.input,
            nameId: 'organizationNameSettingsInput',
            getValue: context => context.organization?.name ?? '',
            setValue: async (context, newName, reset) => {
              const oldName = context.organization?.name ?? null
              if (oldName !== newName) {
                await context.updateOrganization([{ name: newName }]).catch(reset)
              }
            },
            validate: name => (/\S/.test(name) ? true : ''),
            getEditable: () => true,
          },
          {
            type: SettingsEntryType.input,
            nameId: 'organizationEmailSettingsInput',
            getValue: context => context.organization?.email ?? '',
            setValue: async (context, newValue, reset) => {
              const newEmail = backend.EmailAddress(newValue)
              const oldEmail = context.organization?.email ?? null
              if (oldEmail !== newEmail) {
                await context.updateOrganization([{ email: newEmail }]).catch(reset)
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
            type: SettingsEntryType.input,
            nameId: 'organizationWebsiteSettingsInput',
            getValue: context => context.organization?.website ?? '',
            setValue: async (context, newValue, reset) => {
              const newWebsite = backend.HttpsUrl(newValue)
              const oldWebsite = context.organization?.website ?? null
              if (oldWebsite !== newWebsite) {
                await context.updateOrganization([{ website: newWebsite }]).catch(reset)
              }
            },
            getEditable: () => true,
          },
          {
            type: SettingsEntryType.input,
            nameId: 'organizationLocationSettingsInput',
            getValue: context => context.organization?.address ?? '',
            setValue: async (context, newLocation, reset) => {
              const oldLocation = context.organization?.address ?? null
              if (oldLocation !== newLocation) {
                await context.updateOrganization([{ address: newLocation }]).catch(reset)
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
            type: SettingsEntryType.custom,
            aliasesId: 'organizationProfilePictureSettingsCustomEntryAliases',
            render: context =>
              context.backend && <OrganizationProfilePictureInput backend={context.backend} />,
          },
        ],
      },
    ],
  },
  [SettingsTabType.members]: {
    nameId: 'membersSettingsTab',
    settingsTab: SettingsTabType.members,
    icon: PeopleIcon,
    organizationOnly: true,
    feature: 'inviteUser',
    sections: [
      {
        nameId: 'membersSettingsSection',
        entries: [
          {
            type: SettingsEntryType.custom,
            render: () => <MembersSettingsSection />,
          },
        ],
      },
    ],
  },
  [SettingsTabType.userGroups]: {
    nameId: 'userGroupsSettingsTab',
    settingsTab: SettingsTabType.userGroups,
    icon: PeopleSettingsIcon,
    organizationOnly: true,
    feature: 'userGroups',
    sections: [
      {
        nameId: 'userGroupsSettingsSection',
        columnClassName: 'h-3/5 lg:h-[unset] overflow-auto',
        entries: [
          {
            type: SettingsEntryType.custom,
            render: context =>
              context.backend && <UserGroupsSettingsSection backend={context.backend} />,
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
            render: context =>
              context.backend && (
                <MembersTable backend={context.backend} draggable populateWithSelf />
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
  [SettingsTabType.activityLog]: {
    nameId: 'activityLogSettingsTab',
    settingsTab: SettingsTabType.activityLog,
    icon: LogIcon,
    organizationOnly: true,
    sections: [
      {
        nameId: 'activityLogSettingsSection',
        entries: [
          {
            type: SettingsEntryType.custom,
            render: context =>
              context.backend && <ActivityLogSettingsSection backend={context.backend} />,
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
    ],
  },
  {
    nameId: 'accessSettingsTabSection',
    tabs: [
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
  readonly updateUser: reactQuery.UseMutateAsyncFunction<
    void,
    Error,
    Parameters<Backend['updateUser']>,
    unknown
  >
  readonly backend: Backend | null
  readonly organization: backend.OrganizationInfo | null
  readonly updateOrganization: reactQuery.UseMutateAsyncFunction<
    backend.OrganizationInfo | null | undefined,
    Error,
    Parameters<Backend['updateOrganization']>,
    unknown
  >
  readonly toastAndLog: toastAndLogHooks.ToastAndLogCallback
  readonly getText: textProvider.GetText
}

// ==============================
// === SettingsInputEntryData ===
// ==============================

/** Metadata describing a settings entry that is an input. */
export interface SettingsInputEntryData {
  readonly type: SettingsEntryType.input
  readonly nameId: text.TextId & `${string}SettingsInput`
  readonly getValue: (context: SettingsContext) => string
  readonly setValue: (context: SettingsContext, value: string, reset: () => void) => Promise<void>
  readonly validate?: (value: string, context: SettingsContext) => string | true
  readonly getEditable: (context: SettingsContext) => boolean
}

// ===============================
// === SettingsCustomEntryData ===
// ===============================

/** Metadata describing a settings entry that needs custom rendering. */
export interface SettingsCustomEntryData {
  readonly type: SettingsEntryType.custom
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
  readonly organizationOnly?: true
  /** The feature behind which this settings tab is locked. If the user cannot access the feature,
   * a paywall is shown instead of the settings tab. */
  readonly feature?: billing.PaywallFeatureName
  readonly sections: readonly SettingsSectionData[]
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
