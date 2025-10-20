/**
 * @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components.
 */
import Page from '#/components/Page'
import { backendQueryOptions } from '#/hooks/backendHooks'
import { usePaywall } from '#/hooks/billing'
import { useBindGlobalActions } from '#/hooks/menuHooks'
import { CategoriesProvider } from '#/layouts/Drive/Categories'
import SettingsTabType from '#/layouts/Settings/TabType'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as backendModule from '#/services/Backend'
import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'
import { vueComponent } from '#/utilities/vue'
import { SEARCH_PARAMS_PREFIX } from '$/appUtils'
// eslint-disable-next-line no-restricted-syntax
import AppContainerVue from '$/components/AppContainer'
import { useBackends, useFullUserSession, useRouter } from '$/providers/react'
import { useQuery } from '@tanstack/react-query'
import * as detect from 'enso-common/src/detect'
import * as React from 'react'
import type { Router } from 'vue-router'

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

  return (
    <CategoriesProvider>
      <Page hideInfoBar>
        <div
          className="flex min-h-full flex-col text-xs text-primary"
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
