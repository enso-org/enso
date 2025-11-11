/**
 * @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components.
 */
import { Dialog } from '#/components/Dialog'
import Page from '#/components/Page'
import { Text } from '#/components/Text'
import { backendQueryOptions } from '#/hooks/backendHooks'
import { usePaywall } from '#/hooks/billing'
import { useBindGlobalActions } from '#/hooks/menuHooks'
import { CategoriesProvider } from '#/layouts/Drive/Categories'
import SettingsTabType from '#/layouts/Settings/TabType'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'
import { vueComponent } from '#/utilities/vue'
import { SEARCH_PARAMS_PREFIX } from '$/appUtils'
import { useBackends, useFullUserSession, useRouter, useText } from '$/providers/react'
import { useVueValue } from '$/providers/react/common'
import { useOpenedProjects } from '$/providers/react/openedProjects'
import { useQuery } from '@tanstack/react-query'
import * as backendModule from 'enso-common/src/services/Backend'
import * as detect from 'enso-common/src/utilities/detect'
import * as React from 'react'
import type { Router } from 'vue-router'
// eslint-disable-next-line no-restricted-syntax
import AppContainerVue from '$/components/AppContainer'

// This is a component, not a mere constant
// eslint-disable-next-line no-restricted-syntax
const AppContainerInner = vueComponent(AppContainerVue).default

/** Navigate to a specific settings tab. */
function goToSettingsTab(router: Router, tab: SettingsTabType) {
  void router.push({
    path: '/settings',
    query: { [`${SEARCH_PARAMS_PREFIX}SettingsTab`]: JSON.stringify(tab) },
  })
}

/** The component that contains the entire UI. */
export function Dashboard() {
  const { remoteBackend, localBackend } = useBackends()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const { router } = useRouter()
  const { data: organization = null } = useQuery(
    backendQueryOptions(remoteBackend, 'getOrganization', []),
  )
  const { user } = useFullUserSession()
  const { isFeatureUnderPaywall } = usePaywall({ plan: user.plan })
  const openedProjects = useOpenedProjects()
  const closingOnAppExit = useVueValue(
    React.useCallback(() => openedProjects.closingOnAppExit.value, [openedProjects]),
  )

  const inputBindingHandlers = React.useMemo(() => {
    const hasOrganization = backendModule.isUserOnPlanWithMultipleSeats(user)

    return inputBindings.defineHandlers({
      // We want to handle the back and forward buttons in electron the same way as in the browser.
      ...(detect.isOnElectron() && {
        goBack: () => {
          window.api?.navigation.goBack()
        },
        goForward: () => {
          window.api?.navigation.goForward()
        },
        goToAccountSettings: () => {
          goToSettingsTab(router, SettingsTabType.account)
        },
        ...(hasOrganization && {
          goToOrganizationSettings: () => {
            goToSettingsTab(router, SettingsTabType.organization)
          },
        }),
        ...(localBackend && {
          goToLocalSettings: () => {
            goToSettingsTab(router, SettingsTabType.local)
          },
        }),
        ...(user.isOrganizationAdmin &&
          organization?.subscription != null && {
            goToBillingAndPlansSettings: () => {
              goToSettingsTab(router, SettingsTabType.billingAndPlans)
            },
          }),
        ...(hasOrganization && {
          goToMembersSettings: () => {
            goToSettingsTab(router, SettingsTabType.members)
          },
        }),
        ...(hasOrganization && {
          goToUserGroupsSettings: () => {
            goToSettingsTab(router, SettingsTabType.userGroups)
          },
        }),
        goToKeyboardShortcutsSettings: () => {
          goToSettingsTab(router, SettingsTabType.keyboardShortcuts)
        },
        ...(hasOrganization && {
          goToActivityLogSettings: () => {
            goToSettingsTab(router, SettingsTabType.activityLog)
          },
        }),
      }),
      closeModal: () => modalProvider.unsetModal(),
    })
  }, [inputBindings, localBackend, organization?.subscription, router, user])

  useBindGlobalActions(inputBindingHandlers)

  React.useEffect(
    () =>
      inputBindings.attach(sanitizedEventTargets.document.body, 'keydown', inputBindingHandlers),
    [inputBindings, inputBindingHandlers],
  )

  React.useEffect(() => {
    if (closingOnAppExit) {
      modalProvider.setModal(<SyncingProjectsDialog />)
    } else {
      modalProvider.unsetModal()
    }
  }, [closingOnAppExit])

  return (
    <CategoriesProvider>
      <Page hideInfoBar hideModalWrapper>
        <div
          className="flex h-full flex-col text-xs text-primary"
          onContextMenu={(event) => {
            event.preventDefault()
            modalProvider.unsetModal()
          }}
        >
          <AppContainerInner isFeatureUnderPaywall={isFeatureUnderPaywall} />
        </div>
      </Page>
    </CategoriesProvider>
  )
}

/** A dialog informing user that some hybrid projects are uploaded after closing app. */
function SyncingProjectsDialog() {
  const { getText } = useText()
  return (
    <Dialog
      title={getText('syncingProjectsTitle')}
      isDismissable={false}
      hideCloseButton={true}
      modalProps={{ defaultOpen: true }}
    >
      <Text>{getText('syncingProjectsMessage')}</Text>
    </Dialog>
  )
}
