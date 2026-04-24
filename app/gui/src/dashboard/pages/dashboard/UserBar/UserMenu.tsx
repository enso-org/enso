/** @file A dropdown menu of user actions and settings. */
import { Popover } from '#/components/Dialog'
import MenuEntry from '#/components/MenuEntry'
import { ProfilePicture } from '#/components/ProfilePicture'
import { Text } from '#/components/Text'
import { backendMutationOptions } from '#/hooks/backendHooks'
import { useMenuEntries } from '#/hooks/menuHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import { AboutModal } from '#/modals/AboutModal'
import { unsetModal } from '#/providers/ModalProvider'
import { download } from '#/utilities/download'
import { getDownloadUrl } from '#/utilities/github'
import { twMerge } from '#/utilities/tailwindMerge'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { SUBSCRIBE_PATH } from '$/appUtils'
import { useBackends, useFullUserSession, useRouter, useSession, useText } from '$/providers/react'
import { useShowEnsoDevtools } from '$/providers/react/devTools'
import { NetworkError, Plan } from 'enso-common/src/services/Backend'
import { IS_DEV_MODE } from 'enso-common/src/utilities/detect'
import { toast } from 'react-toastify'

/** Props for a {@link UserMenu}. */
export interface UserMenuProps {
  readonly goToSettingsPage: () => void
  readonly onSignOut: () => void
}

/** A dropdown menu of user actions and settings. */
export function UserMenu(props: UserMenuProps) {
  const { goToSettingsPage, onSignOut } = props

  const { router } = useRouter()
  const { localBackend, remoteBackend } = useBackends()
  const { signOut } = useSession()
  const { user } = useFullUserSession()
  const { getText } = useText()
  const toastAndLog = useToastAndLog()
  const [showEnsoDevtools, setShowEnsoDevtools] = useShowEnsoDevtools()
  const updateUser = useMutationCallback(backendMutationOptions(remoteBackend, 'updateUser'))

  const userOrganizations = user.organizations ?? []
  const organizationsSwitcher = useMenuEntries(
    userOrganizations
      .filter((organization) => organization.name != null)
      .map((organization) => {
        return {
          isSelected: user.organizationId === organization.id,
          action: 'switchOrganization',
          doAction: () => {
            if (user.organizationId !== organization.id) {
              const update = updateUser([
                { organizationId: organization.id, switchOrganization: true },
              ])
              void toast.promise(update, {
                success: getText('organizationSwitched'),
                pending: getText('switchingOrganization'),
                error: {
                  render: ({ data: err }) =>
                    err instanceof NetworkError ?
                      err.message
                    : getText('switchingOrganizationError'),
                },
              })
            }
          },
          label: `${organization.name} (${user.organizationId === organization.id ? 'current' : 'switch'})`,
          truncateLabel: true,
          picture: (
            <ProfilePicture
              size="xsmall"
              picture={organization.picture}
              name={organization.name || ''}
            />
          ),
        }
      }),
  )

  const entries = useMenuEntries([
    localBackend == null && {
      action: 'downloadApp',
      doAction: () => {
        unsetModal()
        void getDownloadUrl().then((downloadUrl) => {
          if (downloadUrl == null) {
            toastAndLog('noAppDownloadError')
          } else {
            void download({ url: downloadUrl })
          }
        })
      },
    },
    { action: 'settings', doAction: goToSettingsPage },
    {
      action: 'aboutThisApp',
      doAction: () => {
        AboutModal.open()
      },
    },
    user.isEnsoTeamMember &&
      IS_DEV_MODE && {
        action: 'toggleEnsoDevtools',
        doAction: () => {
          setShowEnsoDevtools(!showEnsoDevtools)
        },
      },
    (user.plan === Plan.free || user.plan === Plan.solo) && {
      action: 'upgradePlan',
      doAction: () => {
        onSignOut()
        void router.push(SUBSCRIBE_PATH)
      },
    },
  ])

  const tailEntries = useMenuEntries([
    {
      action: 'signOut',
      doAction: () => {
        onSignOut()
        void signOut()
      },
    },
  ])

  return (
    <>
      <Popover data-testid="user-menu" size="xxsmall">
        <div className="mb-2 flex select-none items-center gap-icons overflow-hidden px-menu-entry transition-all duration-user-menu">
          <ProfilePicture picture={user.profilePicture} name={user.name} />
          <div className="flex min-w-0 flex-col">
            <Text disableLineHeightCompensation variant="body" truncate="1" weight="semibold">
              {user.name}
            </Text>
            <Text disableLineHeightCompensation>{getText(user.plan)}</Text>
          </div>
        </div>

        {user.maintainerAccount && (
          <div className="-mx-1.5 mb-1 flex flex-col overflow-hidden">
            {organizationsSwitcher.map(
              (entry, index) =>
                entry && (
                  <div
                    key={`${entry.action}-${index}`}
                    className={twMerge(
                      'px-1.5 transition-colors',
                      entry.isSelected ? 'bg-hover-bg hover:bg-black-a16' : 'hover:bg-hover-bg',
                    )}
                  >
                    <MenuEntry {...entry} hasHoverBackground={false} />
                  </div>
                ),
            )}
          </div>
        )}

        <div className="flex flex-col overflow-hidden">
          {entries.map((entry) => entry && <MenuEntry key={entry.action} {...entry} />)}
        </div>

        <div className="flex flex-col overflow-hidden">
          {tailEntries.map((entry) => entry && <MenuEntry key={entry.action} {...entry} />)}
        </div>
      </Popover>
    </>
  )
}
