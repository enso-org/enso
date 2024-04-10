/** @file Settings tab for viewing and editing roles for all users in the organization. */
import * as React from 'react'

import * as mimeTypes from '#/data/mimeTypes'

import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'
import * as refreshHooks from '#/hooks/refreshHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import MembersTable from '#/layouts/Settings/MembersTable'

import * as aria from '#/components/aria'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'
import HorizontalMenuBar from '#/components/styled/HorizontalMenuBar'
import SettingsSection from '#/components/styled/settings/SettingsSection'
import UnstyledButton from '#/components/UnstyledButton'

import NewUserGroupModal from '#/modals/NewUserGroupModal'

import * as backendModule from '#/services/Backend'

// ==============================
// === MemberRolesSettingsTab ===
// ==============================

/** Settings tab for viewing and editing organization members. */
export default function MemberRolesSettingsTab() {
  const { backend } = backendProvider.useBackend()
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const [refresh, doRefresh] = refreshHooks.useRefresh()
  const userGroups = asyncEffectHooks.useAsyncEffect(null, () => backend.listUserGroups(), [
    backend,
    refresh,
  ])
  // NOTE: Neither users nor user groups currently return the information needed to list users
  // within a group.
  const users = asyncEffectHooks.useAsyncEffect(null, () => backend.listUsers(), [backend])
  const isLoading = userGroups == null || users == null
  const { dragAndDropHooks } = aria.useDragAndDrop({
    getDropOperation: (target, types, allowedOperations) =>
      allowedOperations.includes('copy') &&
      types.has(mimeTypes.USER_MIME_TYPE) &&
      target.type === 'item' &&
      typeof target.key === 'string' &&
      backendModule.isUserGroupId(target.key)
        ? 'copy'
        : 'cancel',
    onItemDrop: event => {
      if (typeof event.target.key === 'string' && backendModule.isUserGroupId(event.target.key)) {
        const userGroupId = event.target.key
        for (const item of event.items) {
          if (item.kind === 'text' && item.types.has(mimeTypes.USER_MIME_TYPE)) {
            void item.getText(mimeTypes.USER_MIME_TYPE).then(async text => {
              // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
              const user: backendModule.User = JSON.parse(text)
              const groups = user.userGroups ?? []
              if (!groups.includes(userGroupId)) {
                await backend.changeUserGroup(user.userId, [...groups, userGroupId], user.name)
              }
            })
          }
        }
      }
    },
  })

  return (
    <div className="flex h flex-col gap-settings-section lg:h-auto lg:flex-row">
      <div className="flex w-settings-main-section flex-col gap-settings-subsection">
        <SettingsSection noFocusArea title={getText('userGroups')}>
          <HorizontalMenuBar>
            <UnstyledButton
              className="flex h-row items-center rounded-full bg-frame px-new-project-button-x"
              onPress={() => {
                setModal(<NewUserGroupModal onSubmit={doRefresh} />)
              }}
            >
              <aria.Text className="text whitespace-nowrap font-semibold">
                {getText('newUserGroup')}
              </aria.Text>
            </UnstyledButton>
          </HorizontalMenuBar>
          <aria.Table
            aria-label={getText('userGroups')}
            className="table-fixed self-start rounded-rows"
            dragAndDropHooks={dragAndDropHooks}
          >
            <aria.TableHeader className="h-row">
              <aria.Column
                isRowHeader
                className="w-members-name-column border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0"
              >
                {getText('userGroup')}
              </aria.Column>
            </aria.TableHeader>
            <aria.TableBody className="select-text">
              {isLoading ? (
                <aria.Row className="h-row">
                  <aria.Cell
                    ref={element => {
                      if (element != null) {
                        element.colSpan = 3
                      }
                    }}
                    className="bg-transparent"
                  >
                    <div className="flex justify-center">
                      <StatelessSpinner
                        size={32}
                        state={statelessSpinner.SpinnerState.loadingMedium}
                      />
                    </div>
                  </aria.Cell>
                </aria.Row>
              ) : (
                userGroups.flatMap(userGroup => [
                  <aria.Row key={userGroup.id} id={userGroup.id} className="h-row">
                    <aria.Cell className="text border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0">
                      {userGroup.groupName}
                    </aria.Cell>
                  </aria.Row>,
                ])
              )}
            </aria.TableBody>
          </aria.Table>
        </SettingsSection>
      </div>
      <SettingsSection noFocusArea title={getText('users')}>
        <MembersTable draggable />
      </SettingsSection>
    </div>
  )
}
