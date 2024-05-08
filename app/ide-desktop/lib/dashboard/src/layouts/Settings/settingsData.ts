;

/** @file Metadata for rendering each settings section. */
import KeyboardShortcutsIcon from 'enso-assets/keyboard_shortcuts.svg';
import LogIcon from 'enso-assets/log.svg';
import PeopleSettingsIcon from 'enso-assets/people_settings.svg';
import PeopleIcon from 'enso-assets/people.svg';
import SettingsIcon from 'enso-assets/settings.svg';



import type * as text from '#/text';



import type * as toastAndLogHooks from '#/hooks/toastAndLogHooks';



import ActivityLogSettingsSection from '#/layouts/Settings/ActivityLogSettingsSection';
import ChangePasswordForm from '#/layouts/Settings/ChangePasswordForm';
import DeleteUserAccountSettingsSection from '#/layouts/Settings/DeleteUserAccountSettingsSection';
import KeyboardShortcutsSettingsSection from '#/layouts/Settings/KeyboardShortcutsSettingsSection';
import MembersSettingsSection from '#/layouts/Settings/MembersSettingsSection';
import OrganizationProfilePictureInput from '#/layouts/Settings/OrganizationProfilePictureInput';
import ProfilePictureInput from '#/layouts/Settings/ProfilePictureInput';
import SettingsTabType from '#/layouts/Settings/SettingsTabType';



import * as backend from '#/services/Backend';
import type Backend from '#/services/Backend';



import * as object from '#/utilities/object';





;








































;







































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
            getValue: context => context.user?.name ?? '',
            setValue: async (context, newName, reset) => {
              const oldName = context.user?.name ?? ''
              if (newName === oldName) {
                return
              } else {
                try {
                  context.setUser(object.merger({ name: newName }))
                  await context.backend.updateUser({ username: newName })
                } catch (error) {
                  context.setUser(object.merger({ name: oldName }))
                  context.toastAndLog(null, error)
                  reset()
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
            aliasesId: 'changePasswordSettingsCustomEntryAliases',
            render: ChangePasswordForm,
            getVisible: context => {
              // The shape of the JWT payload is statically known.
              // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
              const username: string | null =
                context.accessToken != null
                  ? // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-non-null-assertion
                    JSON.parse(atob(context.accessToken.split('.')[1]!)).username
                  : null
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
            render: DeleteUserAccountSettingsSection,
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
            render: ProfilePictureInput,
          },
        ],
      },
    ],
  },
  [SettingsTabType.organization]: {
    nameId: 'organizationSettingsTab',
    settingsTab: SettingsTabType.organization,
    icon: PeopleSettingsIcon,
    sections: [
      {
        nameId: 'organizationSettingsSection',
        entries: [
          {
            type: SettingsEntryType.input,
            nameId: 'organizationNameSettingsInput',
            getValue: context => context.organization.name ?? '',
            setValue: async (context, newName, reset) => {
              const oldName = context.organization.name ?? null
              if (oldName !== newName) {
                try {
                  // eslint-disable-next-line @typescript-eslint/naming-convention
                  context.setOrganization(object.merger({ name: newName }))
                  const newOrganization = await context.backend.updateOrganization({
                    name: newName,
                  })
                  if (newOrganization != null) {
                    context.setOrganization(newOrganization)
                  }
                } catch (error) {
                  // eslint-disable-next-line @typescript-eslint/naming-convention
                  context.setOrganization(object.merger({ name: oldName }))
                  context.toastAndLog(null, error)
                  reset()
                }
              }
            },
            getEditable: () => true,
          },
          {
            type: SettingsEntryType.input,
            nameId: 'organizationEmailSettingsInput',
            getValue: context => context.organization.email ?? '',
            setValue: async (context, newValue, reset) => {
              const newEmail = backend.EmailAddress(newValue)
              const oldEmail = context.organization.email ?? null
              if (oldEmail !== newEmail) {
                try {
                  // eslint-disable-next-line @typescript-eslint/naming-convention
                  context.setOrganization(object.merger({ email: newEmail }))
                  const newOrganization = await context.backend.updateOrganization({
                    email: newEmail,
                  })
                  if (newOrganization != null) {
                    context.setOrganization(newOrganization)
                  }
                } catch (error) {
                  // eslint-disable-next-line @typescript-eslint/naming-convention
                  context.setOrganization(object.merger({ email: oldEmail }))
                  context.toastAndLog(null, error)
                  reset()
                }
              }
            },
            getEditable: () => true,
          },
          {
            type: SettingsEntryType.input,
            nameId: 'organizationWebsiteSettingsInput',
            getValue: context => context.organization.website ?? '',
            setValue: async (context, newValue, reset) => {
              const newWebsite = backend.HttpsUrl(newValue)
              const oldWebsite = context.organization.website ?? null
              if (oldWebsite !== newWebsite) {
                try {
                  // eslint-disable-next-line @typescript-eslint/naming-convention
                  context.setOrganization(object.merger({ website: newWebsite }))
                  const newOrganization = await context.backend.updateOrganization({
                    website: newWebsite,
                  })
                  if (newOrganization != null) {
                    context.setOrganization(newOrganization)
                  }
                } catch (error) {
                  // eslint-disable-next-line @typescript-eslint/naming-convention
                  context.setOrganization(object.merger({ website: oldWebsite }))
                  context.toastAndLog(null, error)
                  reset()
                }
              }
            },
            getEditable: () => true,
          },
          {
            type: SettingsEntryType.input,
            nameId: 'organizationLocationSettingsInput',
            getValue: context => context.organization.address ?? '',
            setValue: async (context, newLocation, reset) => {
              const oldLocation = context.organization.address ?? null
              if (oldLocation !== newLocation) {
                try {
                  // eslint-disable-next-line @typescript-eslint/naming-convention
                  context.setOrganization(object.merger({ address: newLocation }))
                  const newOrganization = await context.backend.updateOrganization({
                    address: newLocation,
                  })
                  if (newOrganization != null) {
                    context.setOrganization(newOrganization)
                  }
                } catch (error) {
                  // eslint-disable-next-line @typescript-eslint/naming-convention
                  context.setOrganization(object.merger({ address: oldLocation }))
                  context.toastAndLog(null, error)
                  reset()
                }
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
            render: OrganizationProfilePictureInput,
          },
        ],
      },
    ],
  },
  [SettingsTabType.members]: {
    nameId: 'membersSettingsTab',
    settingsTab: SettingsTabType.members,
    icon: PeopleIcon,
    sections: [
      {
        nameId: 'membersSettingsSection',
        entries: [
          {
            type: SettingsEntryType.custom,
            render: MembersSettingsSection,
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
    sections: [
      {
        nameId: 'activityLogSettingsSection',
        entries: [
          {
            type: SettingsEntryType.custom,
            render: ActivityLogSettingsSection,
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
    tabs: [SETTINGS_TAB_DATA[SettingsTabType.members]],
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

// =======================
// === SettingsContext ===
// =======================

/** Metadata describing inputs passed to every settings entry. */
export interface SettingsContext {
  readonly accessToken: string | null
  readonly user: backend.User | null
  readonly setUser: React.Dispatch<React.SetStateAction<backend.User>>
  readonly organization: backend.OrganizationInfo
  readonly setOrganization: React.Dispatch<React.SetStateAction<backend.OrganizationInfo>>
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
  readonly setValue: (context: SettingsContext, value: string, reset: () => void) => Promise<void>
  readonly getEditable: (context: SettingsContext) => boolean
}

// ===============================
// === SettingsCustomEntryData ===
// ===============================

/** Metadata describing a settings entry that needs custom rendering. */
export interface SettingsCustomEntryData {
  readonly type: SettingsEntryType.custom
  readonly aliasesId?: text.TextId & `${string}SettingsCustomEntryAliases`
  readonly render: (context: SettingsContext) => JSX.Element
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

/** Metadata describing a settings tab. */
export interface SettingsSectionData {
  readonly nameId: text.TextId & `${string}SettingsSection`
  /** The first column is column 1, not column 0. */
  readonly column?: number
  readonly heading?: false
  readonly focusArea?: false
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