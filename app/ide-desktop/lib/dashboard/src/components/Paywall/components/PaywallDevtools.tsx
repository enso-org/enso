/**
 * @file
 *
 * A component that provides a UI for toggling paywall features.
 */
import * as React from 'react'

import DevtoolsLogo from 'enso-assets/enso_logo.svg'

import * as billing from '#/hooks/billing'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import Portal from '#/components/Portal'

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
 * Props for the {@link PaywallDevtools} component.
 */
interface PaywallDevtoolsProps extends React.PropsWithChildren {}

/**
 * A component that provides a UI for toggling paywall features.
 */
export function PaywallDevtools(props: PaywallDevtoolsProps) {
  const { children } = props

  const { getText } = textProvider.useText()

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
            size="large"
            className="fixed bottom-16 right-4 z-50"
            data-ignore-click-outside
          />

          <ariaComponents.Popover>
            <ariaComponents.Text.Heading>
              {getText('paywallDevtoolsPopoverHeading')}
            </ariaComponents.Text.Heading>

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
                      <div className="box-border flex h-[14px] w-[22px] shrink-0 cursor-default rounded-full border border-solid border-white/30 bg-yellow-600 bg-clip-padding p-[2px] shadow-inner outline-none ring-black transition duration-200 ease-in-out group-focus-visible:ring-2 group-pressed:bg-yellow-700 group-selected:bg-amber-800 group-selected:group-pressed:bg-amber-900">
                        <span className="h-2 w-2 translate-x-0 transform rounded-full bg-white shadow transition duration-200 ease-in-out group-selected:translate-x-[100%]" />
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
  return React.useContext(PaywallDevtoolsContext)
}
