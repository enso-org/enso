/** @file Metadata for rendering each settings section. */
import * as React from 'react'

import type * as reactQuery from '@tanstack/react-query'
import isEmail from 'validator/lib/isEmail'

import type * as text from 'enso-common/src/text'

import ComputerIcon from '#/assets/computer.svg'
import CreditCardIcon from '#/assets/credit_card.svg'
import KeyboardShortcutsIcon from '#/assets/keyboard_shortcuts.svg'
import LogIcon from '#/assets/log.svg'
import PeopleIcon from '#/assets/people.svg'
import PeopleSettingsIcon from '#/assets/people_settings.svg'
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
import SettingsTabType from '#/layouts/Settings/SettingsTabType'
import UserGroupsSettingsSection from '#/layouts/Settings/UserGroupsSettingsSection'

import { Button, ButtonGroup } from '#/components/AriaComponents'
import * as menuEntry from '#/components/MenuEntry'

import type Backend from '#/services/Backend'
import * as backend from '#/services/Backend'
import type LocalBackend from '#/services/LocalBackend'
import type RemoteBackend from '#/services/RemoteBackend'

import * as object from '#/utilities/object'
import * as z from 'zod'

// =========================
// === SettingsEntryType ===
// =========================

// =================
// === Constants ===
// =================

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
    icon: SettingsIcon,
    sections: [
      {
        nameId: 'userAccountSettingsSection',
        entries: [
          settingsFormEntryData({
            type: 'form',
            schema: z.object({
              username: z.string().regex(/.*\S.*/),
              email: z.string().email(),
            }),
            onSubmit: async (context, { username }) => {
              const oldName = context.user.name
              if (username !== oldName) {
                await context.updateUser([{ username }])
              }
            },
            inputs: [
              {
                nameId: 'userNameSettingsInput',
                name: 'username',
                getValue: (context) => context.user.name,
                getEditable: () => true,
              },
              {
                nameId: 'userEmailSettingsInput',
                name: 'email',
                getValue: (context) => context.user.email,
                getEditable: () => false,
              },
            ],
          }),
        ],
      },
      {
        nameId: 'changePasswordSettingsSection',
        entries: [
          {
            type: 'custom',
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
    icon: PeopleSettingsIcon,
    organizationOnly: true,
    sections: [
      {
        nameId: 'organizationSettingsSection',
        entries: [
          settingsFormEntryData({
            type: 'form',
            schema: z.object({
              name: z.string().regex(/.*\S.*/),
              email: z.string().email(),
              website: z.string(),
              address: z.string(),
            }),
            onSubmit: async (context, { name, email, website, address }) => {
              await context.updateOrganization([
                {
                  name,
                  email: backend.EmailAddress(email),
                  website: backend.HttpsUrl(website),
                  address,
                },
              ])
            },
            inputs: [
              {
                nameId: 'organizationNameSettingsInput',
                name: 'name',
                getValue: (context) => context.organization?.name ?? '',
                getEditable: (context) => context.user.isOrganizationAdmin,
              },
              {
                nameId: 'organizationEmailSettingsInput',
                name: 'email',
                getValue: (context) => context.organization?.email ?? '',
                getEditable: (context) => context.user.isOrganizationAdmin,
              },
              {
                nameId: 'organizationWebsiteSettingsInput',
                name: 'website',
                getValue: (context) => context.organization?.website ?? '',
                getEditable: (context) => context.user.isOrganizationAdmin,
              },
              {
                nameId: 'organizationLocationSettingsInput',
                name: 'address',
                getValue: (context) => context.organization?.address ?? '',
                getEditable: (context) => context.user.isOrganizationAdmin,
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
    icon: ComputerIcon,
    visible: (context) => context.localBackend != null,
    sections: [
      {
        nameId: 'localSettingsSection',
        entries: [
          settingsFormEntryData({
            type: 'form',
            schema: z.object({
              localRootPath: z.string(),
            }),
            onSubmit: (context, { localRootPath }) => {
              context.updateLocalRootPath(localRootPath)
            },
            inputs: [
              {
                nameId: 'localRootPathSettingsInput',
                name: 'localRootPath',
                getValue: (context) => context.localBackend?.rootPath ?? '',
                getEditable: () => true,
              },
            ],
          }),
          {
            type: 'custom',
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
                        context.updateLocalRootPath(newDirectory)
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
    visible: (context) => context.organization?.subscription != null,
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
    feature: 'inviteUser',
    sections: [
      {
        nameId: 'membersSettingsSection',
        entries: [{ type: 'custom', render: () => <MembersSettingsSection /> }],
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
            type: 'custom',
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
            type: 'custom',
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
            type: 'custom',
            aliasesId: 'keyboardShortcutsSettingsCustomEntryAliases',
            getExtraAliases: (context) => {
              const rebindableBindings = object
                .unsafeEntries(inputBindings.BINDINGS)
                .flatMap((kv) => {
                  const [k, v] = kv
                  if (v.rebindable === false) {
                    return []
                  } else {
                    return menuEntry.ACTION_TO_TEXT_ID[k]
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
    sections: [
      {
        nameId: 'activityLogSettingsSection',
        entries: [
          {
            type: 'custom',
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
  readonly user: backend.User
  readonly backend: RemoteBackend
  readonly localBackend: LocalBackend | null
  readonly organization: backend.OrganizationInfo | null
  readonly updateUser: (variables: Parameters<Backend['updateUser']>) => Promise<void>
  readonly updateOrganization: (
    variables: Parameters<Backend['updateOrganization']>,
  ) => Promise<backend.OrganizationInfo | null | undefined>
  readonly updateLocalRootPath: (rootPath: string) => void
  readonly resetLocalRootPath: () => void
  readonly toastAndLog: toastAndLogHooks.ToastAndLogCallback
  readonly getText: textProvider.GetText
  readonly queryClient: reactQuery.QueryClient
}

// ==============================
// === SettingsInputEntryData ===
// ==============================

/** Metadata describing a settings entry that is an input. */
export interface SettingsInputData<T> {
  readonly nameId: text.TextId & `${string}SettingsInput`
  readonly name: keyof T
  readonly getValue: (context: SettingsContext) => string
  readonly getEditable: (context: SettingsContext) => boolean
}

export interface SettingsFormEntryData<T extends Record<keyof T, string>> {
  readonly type: 'form'
  readonly schema: z.ZodType<T> | ((context: SettingsContext) => z.ZodType<T>)
  readonly onSubmit: (context: SettingsContext, value: T) => void | Promise<void>
  readonly inputs: readonly SettingsInputData<NoInfer<T>>[]
}

function settingsFormEntryData<T extends Record<keyof T, string>>(data: SettingsFormEntryData<T>) {
  return data
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
export type SettingsEntryData = SettingsCustomEntryData | SettingsFormEntryData<any>

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
