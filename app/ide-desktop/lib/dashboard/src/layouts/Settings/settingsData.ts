/** @file Metadata for rendering each settings section. */
import KeyboardShortcutsIcon from 'enso-assets/keyboard_shortcuts.svg'
import LogIcon from 'enso-assets/log.svg'
import PeopleSettingsIcon from 'enso-assets/people_settings.svg'
import PeopleIcon from 'enso-assets/people.svg'
import SettingsIcon from 'enso-assets/settings.svg'

import type * as text from '#/text'

import type * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import ChangePasswordForm from '#/layouts/Settings/ChangePasswordForm'
import DeleteUserAccountSettingsSection from '#/layouts/Settings/DeleteUserAccountSettingsSection'
import ProfilePictureInput from '#/layouts/Settings/ProfilePictureInput'
import SettingsTab from '#/layouts/Settings/SettingsTab'

import type * as backend from '#/services/Backend'
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

export const DATA: SettingsData = [
  {
    nameId: 'generalSettingsTabSection',
    tabs: [
      {
        nameId: 'accountSettingsTab',
        settingsTab: SettingsTab.account,
        icon: SettingsIcon,
        sections: [
          {
            nameId: 'userAccountSettingsSection',
            entries: [
              {
                type: SettingsEntryType.input,
                nameId: 'userNameSettingsInput',
                getValue: context => context.user?.name ?? '',
                setValue: async (context, newName) => {
                  const oldName = context.user?.name ?? ''
                  if (newName === oldName) {
                    return
                  } else {
                    try {
                      await context.backend.updateUser({ username: newName })
                      context.setUser(object.merger({ name: newName }))
                    } catch (error) {
                      context.toastAndLog(null, error)
                    }
                    return
                  }
                },
                getEditable: () => true,
              },
              {
                type: SettingsEntryType.input,
                nameId: 'userEmailSettingsInput',
                getValue: context => context.user?.email ?? '',
                // A user's email currently cannot be changed.
                setValue: async () => {},
                getEditable: () => false,
              },
            ],
          },
          {
            nameId: 'changePasswordSettingsSection',
            entries: [
              {
                type: SettingsEntryType.custom,
                render: ChangePasswordForm,
              },
            ],
          },
          {
            nameId: 'deleteUserAccountSettingsSection',
            heading: false,
            entries: [
              {
                type: SettingsEntryType.custom,
                render: DeleteUserAccountSettingsSection,
              },
            ],
          },
          // TODO: This is in the second column.
          {
            nameId: 'profilePictureSettingsSection',
            entries: [
              {
                type: SettingsEntryType.custom,
                render: ProfilePictureInput,
              },
            ],
          },
        ],
      },
      {
        nameId: 'organizationSettingsTab',
        settingsTab: SettingsTab.organization,
        icon: PeopleSettingsIcon,
        sections: [
          {
            nameId: 'organizationSettingsSection',
          },
        ],
      },
    ],
  },
  {
    nameId: 'accessSettingsTabSection',
    tabs: [
      {
        nameId: 'membersSettingsTab',
        settingsTab: SettingsTab.members,
        icon: PeopleIcon,
      },
    ],
  },
  {
    nameId: 'lookAndFeelSettingsTabSection',
    tabs: [
      {
        nameId: 'keyboardShortcutsSettingsTab',
        settingsTab: SettingsTab.keyboardShortcuts,
        icon: KeyboardShortcutsIcon,
      },
    ],
  },
  {
    nameId: 'securitySettingsTabSection',
    tabs: [
      {
        nameId: 'activityLogSettingsTab',
        settingsTab: SettingsTab.activityLog,
        icon: LogIcon,
      },
    ],
  },
]

// =======================
// === SettingsContext ===
// =======================

/** Metadata describing inputs passed to every settings entry. */
export interface SettingsContext {
  readonly user: backend.User | null
  readonly setUser: React.Dispatch<React.SetStateAction<backend.User>>
  readonly organization: backend.OrganizationInfo
  readonly backend: Backend
  readonly toastAndLog: toastAndLogHooks.ToastAndLogCallback
}

// ==============================
// === SettingsInputEntryData ===
// ==============================

/** Metadata describing a settings entry that is an input. */
export interface SettingsInputEntryData {
  readonly type: SettingsEntryType.input
  readonly nameId: text.TextId & `${string}SettingsInput`
  readonly getValue: (context: SettingsContext) => string
  readonly setValue: (context: SettingsContext, value: string) => Promise<void>
  readonly getEditable: (context: SettingsContext) => boolean
}

// ===============================
// === SettingsCustomEntryData ===
// ===============================

/** Metadata describing a settings entry that needs custom rendering. */
export interface SettingsCustomEntryData {
  readonly type: SettingsEntryType.custom
  readonly render: (context: SettingsContext) => JSX.Element
}

// =========================
// === SettingsEntryData ===
// =========================

/** A settings entry of an arbitrary type. */
type SettingsEntryData = SettingsCustomEntryData | SettingsInputEntryData

// =======================
// === SettingsTabData ===
// =======================

/** Metadata describing a settings tab. */
export interface SettingsSectionData {
  readonly nameId: text.TextId & `${string}SettingsSection`
  readonly heading?: false
  readonly entries: readonly SettingsEntryData[]
}

// =======================
// === SettingsTabData ===
// =======================

/** Metadata describing a settings tab. */
export interface SettingsTabData {
  readonly nameId: text.TextId & `${string}SettingsTab`
  readonly settingsTab: SettingsTab
  readonly icon: string
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
