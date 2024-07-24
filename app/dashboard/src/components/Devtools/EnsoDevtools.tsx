/**
 * @file
 *
 * A component that provides a UI for toggling paywall features.
 */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import { IS_DEV_MODE } from 'enso-common/src/detect'

import DevtoolsLogo from '#/assets/enso_logo.svg'

import * as billing from '#/hooks/billing'

import * as authProvider from '#/providers/AuthProvider'
import {
  useEnableVersionChecker,
  useSetEnableVersionChecker,
} from '#/providers/EnsoDevtoolsProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import Portal from '#/components/Portal'

import * as backend from '#/services/Backend'

/**
 * Configuration for a paywall feature.
 */
export interface PaywallDevtoolsFeatureConfiguration {
  readonly isForceEnabled: boolean | null
}

const PaywallDevtoolsContext = React.createContext<{
  features: Record<billing.PaywallFeatureName, PaywallDevtoolsFeatureConfiguration>
}>({
  features: {
    share: { isForceEnabled: null },
    shareFull: { isForceEnabled: null },
    userGroups: { isForceEnabled: null },
    userGroupsFull: { isForceEnabled: null },
    inviteUser: { isForceEnabled: null },
    inviteUserFull: { isForceEnabled: null },
  },
})

/**
 * Props for the {@link EnsoDevtools} component.
 */
interface EnsoDevtoolsProps extends React.PropsWithChildren {}

/**
 * A component that provides a UI for toggling paywall features.
 */
export function EnsoDevtools(props: EnsoDevtoolsProps) {
  const { children } = props

  const { getText } = textProvider.useText()
  const { authQueryKey } = authProvider.useAuth()
  const session = authProvider.useFullUserSession()
  const enableVersionChecker = useEnableVersionChecker()
  const setEnableVersionChecker = useSetEnableVersionChecker()

  const [features, setFeatures] = React.useState<
    Record<billing.PaywallFeatureName, PaywallDevtoolsFeatureConfiguration>
  >({
    share: { isForceEnabled: null },
    shareFull: { isForceEnabled: null },
    userGroups: { isForceEnabled: null },
    userGroupsFull: { isForceEnabled: null },
    inviteUser: { isForceEnabled: null },
    inviteUserFull: { isForceEnabled: null },
  })

  const { getFeature } = billing.usePaywallFeatures()

  const queryClient = reactQuery.useQueryClient()

  const onConfigurationChange = React.useCallback(
    (feature: billing.PaywallFeatureName, configuration: PaywallDevtoolsFeatureConfiguration) => {
      setFeatures(prev => ({ ...prev, [feature]: configuration }))
    },
    []
  )

  return (
    <PaywallDevtoolsContext.Provider value={{ features }}>
      {children}

      <Portal>
        <ariaComponents.DialogTrigger>
          <ariaComponents.Button
            icon={DevtoolsLogo}
            aria-label={getText('paywallDevtoolsButtonLabel')}
            variant="icon"
            rounded="full"
            size="hero"
            className="fixed bottom-16 right-3 z-50"
            data-ignore-click-outside
          />

          <ariaComponents.Popover>
            <ariaComponents.Text.Heading disableLineHeightCompensation>
              {getText('paywallDevtoolsPopoverHeading')}
            </ariaComponents.Text.Heading>

            <ariaComponents.Separator orientation="horizontal" className="my-3" />

            <ariaComponents.Text variant="subtitle">
              {getText('paywallDevtoolsPlanSelectSubtitle')}
            </ariaComponents.Text>

            <ariaComponents.Form
              gap="small"
              schema={schema => schema.object({ plan: schema.string() })}
              defaultValues={{ plan: session.user.plan ?? 'free' }}
            >
              {({ form }) => (
                <>
                  <ariaComponents.RadioGroup
                    form={form}
                    name="plan"
                    onChange={value => {
                      queryClient.setQueryData(authQueryKey, {
                        ...session,
                        user: { ...session.user, plan: value },
                      })
                    }}
                  >
                    <ariaComponents.Radio label={getText('free')} value="free" />
                    <ariaComponents.Radio label={getText('solo')} value={backend.Plan.solo} />
                    <ariaComponents.Radio label={getText('team')} value={backend.Plan.team} />
                    <ariaComponents.Radio
                      label={getText('enterprise')}
                      value={backend.Plan.enterprise}
                    />
                  </ariaComponents.RadioGroup>

                  <ariaComponents.Button
                    variant="outline"
                    onPress={() =>
                      queryClient.invalidateQueries({ queryKey: authQueryKey }).then(() => {
                        form.reset()
                      })
                    }
                  >
                    {getText('reset')}
                  </ariaComponents.Button>
                </>
              )}
            </ariaComponents.Form>

            <ariaComponents.Separator orientation="horizontal" className="my-3" />

            <ariaComponents.Text variant="subtitle" className="mb-2">
              {getText('productionOnlyFeatures')}
            </ariaComponents.Text>
            <div className="flex flex-col">
              <aria.Switch
                className="group flex items-center gap-1"
                isSelected={enableVersionChecker ?? !IS_DEV_MODE}
                onChange={setEnableVersionChecker}
              >
                <div className="box-border flex h-4 w-[28px] shrink-0 cursor-default items-center rounded-full bg-primary/30 bg-clip-padding p-0.5 shadow-inner outline-none ring-black transition duration-200 ease-in-out group-focus-visible:ring-2 group-pressed:bg-primary/60 group-selected:bg-primary group-selected:group-pressed:bg-primary/50">
                  <span className="aspect-square h-full flex-none translate-x-0 transform rounded-full bg-white transition duration-200 ease-in-out group-selected:translate-x-[100%]" />
                </div>

                <ariaComponents.Text className="flex-1">
                  {getText('enableVersionChecker')}
                </ariaComponents.Text>
              </aria.Switch>

              <ariaComponents.Text variant="body" color="disabled">
                {getText('enableVersionCheckerDescription')}
              </ariaComponents.Text>
            </div>

            <ariaComponents.Separator orientation="horizontal" className="my-3" />

            <ariaComponents.Text variant="subtitle" className="mb-2">
              {getText('paywallDevtoolsPaywallFeaturesToggles')}
            </ariaComponents.Text>

            <div className="flex flex-col gap-1">
              {Object.entries(features).map(([feature, configuration]) => {
                // eslint-disable-next-line no-restricted-syntax
                const featureName = feature as billing.PaywallFeatureName
                const { label, descriptionTextId } = getFeature(featureName)
                return (
                  <div key={feature} className="flex flex-col">
                    <aria.Switch
                      className="group flex items-center gap-1"
                      isSelected={configuration.isForceEnabled ?? true}
                      onChange={value => {
                        onConfigurationChange(featureName, {
                          isForceEnabled: value,
                        })
                      }}
                    >
                      <div className="box-border flex h-4 w-[28px] shrink-0 cursor-default items-center rounded-full bg-primary/30 bg-clip-padding p-0.5 shadow-inner outline-none ring-black transition duration-200 ease-in-out group-focus-visible:ring-2 group-pressed:bg-primary/60 group-selected:bg-primary group-selected:group-pressed:bg-primary/50">
                        <span className="aspect-square h-full flex-none translate-x-0 transform rounded-full bg-white transition duration-200 ease-in-out group-selected:translate-x-[100%]" />
                      </div>

                      <ariaComponents.Text className="flex-1">{getText(label)}</ariaComponents.Text>
                    </aria.Switch>

                    <ariaComponents.Text variant="body" color="disabled">
                      {getText(descriptionTextId)}
                    </ariaComponents.Text>
                  </div>
                )
              })}
            </div>
          </ariaComponents.Popover>
        </ariaComponents.DialogTrigger>
      </Portal>
    </PaywallDevtoolsContext.Provider>
  )
}

/**
 * A hook that provides access to the paywall devtools.
 */
export function usePaywallDevtools() {
  const context = React.useContext(PaywallDevtoolsContext)

  React.useDebugValue(context)

  return context
}
