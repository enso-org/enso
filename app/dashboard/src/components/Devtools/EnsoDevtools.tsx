/**
 * @file
 *
 * A component that provides a UI for toggling paywall features.
 */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import { IS_DEV_MODE } from 'enso-common/src/detect'

import cross from '#/assets/cross.svg'
import DevtoolsLogo from '#/assets/enso_logo.svg'
import trash from '#/assets/trash.svg'

import { SETUP_PATH } from '#/appUtils'

import * as billing from '#/hooks/billing'

import * as authProvider from '#/providers/AuthProvider'
import { UserSessionType } from '#/providers/AuthProvider'
import {
  useEnableVersionChecker,
  useSetEnableVersionChecker,
} from '#/providers/EnsoDevtoolsProvider'
import * as textProvider from '#/providers/TextProvider'

import { Switch } from '#/components/aria'
import {
  Button,
  DialogTrigger,
  Form,
  Popover,
  Radio,
  RadioGroup,
  Separator,
  Text,
} from '#/components/AriaComponents'
import Portal from '#/components/Portal'

import { useLocalStorage } from '#/providers/LocalStorageProvider'
import * as backend from '#/services/Backend'
import LocalStorage from '#/utilities/LocalStorage'
import { unsafeEntries } from 'enso-common/src/utilities/data/object'

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
  const { authQueryKey, session } = authProvider.useAuth()
  const enableVersionChecker = useEnableVersionChecker()
  const setEnableVersionChecker = useSetEnableVersionChecker()
  const { localStorage } = useLocalStorage()

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
      setFeatures((prev) => ({ ...prev, [feature]: configuration }))
    },
    [],
  )

  return (
    <PaywallDevtoolsContext.Provider value={{ features }}>
      {children}

      <Portal>
        <DialogTrigger>
          <Button
            icon={DevtoolsLogo}
            aria-label={getText('paywallDevtoolsButtonLabel')}
            variant="icon"
            rounded="full"
            size="hero"
            className="fixed bottom-16 right-3 z-50"
            data-ignore-click-outside
          />

          <Popover>
            <Text.Heading disableLineHeightCompensation>
              {getText('paywallDevtoolsPopoverHeading')}
            </Text.Heading>

            <Separator orientation="horizontal" className="my-3" />

            {session?.type === UserSessionType.full && (
              <>
                <Text variant="subtitle">{getText('paywallDevtoolsPlanSelectSubtitle')}</Text>

                <Form
                  gap="small"
                  schema={(schema) => schema.object({ plan: schema.string() })}
                  defaultValues={{ plan: session.user.plan ?? 'free' }}
                >
                  {({ form }) => (
                    <>
                      <RadioGroup
                        form={form}
                        name="plan"
                        onChange={(value) => {
                          queryClient.setQueryData(authQueryKey, {
                            ...session,
                            user: { ...session.user, plan: value },
                          })
                        }}
                      >
                        <Radio label={getText('free')} value={'free'} />
                        <Radio label={getText('solo')} value={backend.Plan.solo} />
                        <Radio label={getText('team')} value={backend.Plan.team} />
                        <Radio label={getText('enterprise')} value={backend.Plan.enterprise} />
                      </RadioGroup>

                      <Button
                        size="small"
                        variant="outline"
                        onPress={() =>
                          queryClient.invalidateQueries({ queryKey: authQueryKey }).then(() => {
                            form.reset()
                          })
                        }
                      >
                        {getText('reset')}
                      </Button>
                    </>
                  )}
                </Form>

                <Separator orientation="horizontal" className="my-3" />

                {/* eslint-disable-next-line no-restricted-syntax */}
                <Button variant="link" href={SETUP_PATH + '?__qd-debg__=true'}>
                  Open setup page
                </Button>

                <Separator orientation="horizontal" className="my-3" />
              </>
            )}

            <Text variant="subtitle" className="mb-2">
              {getText('productionOnlyFeatures')}
            </Text>
            <div className="flex flex-col">
              <Switch
                className="group flex items-center gap-1"
                isSelected={enableVersionChecker ?? !IS_DEV_MODE}
                onChange={setEnableVersionChecker}
              >
                <div className="box-border flex h-4 w-[28px] shrink-0 cursor-default items-center rounded-full bg-primary/30 bg-clip-padding p-0.5 shadow-inner outline-none ring-black transition duration-200 ease-in-out group-focus-visible:ring-2 group-pressed:bg-primary/60 group-selected:bg-primary group-selected:group-pressed:bg-primary/50">
                  <span className="aspect-square h-full flex-none translate-x-0 transform rounded-full bg-white transition duration-200 ease-in-out group-selected:translate-x-[100%]" />
                </div>

                <Text className="flex-1">{getText('enableVersionChecker')}</Text>
              </Switch>

              <Text variant="body" color="disabled">
                {getText('enableVersionCheckerDescription')}
              </Text>
            </div>

            <Separator orientation="horizontal" className="my-3" />

            <div className="mb-2 flex w-full items-center justify-between">
              <Text variant="subtitle">{getText('localStorage')}</Text>

              <Button
                aria-label={getText('deleteAll')}
                size="small"
                variant="icon"
                icon={trash}
                onPress={() => {
                  for (const [key] of unsafeEntries(LocalStorage.keyMetadata)) {
                    localStorage.delete(key)
                  }
                }}
              />
            </div>

            <div className="flex flex-col gap-0.5">
              {unsafeEntries(LocalStorage.keyMetadata).map(([key]) => (
                <div key={key} className="flex w-full items-center justify-between gap-1">
                  <Text variant="body">
                    {key
                      .replace(/[A-Z]/g, (m) => ' ' + m.toLowerCase())
                      .replace(/^./, (m) => m.toUpperCase())}
                  </Text>

                  <Button
                    variant="icon"
                    size="small"
                    aria-label={getText('delete')}
                    icon={cross}
                    onPress={() => {
                      localStorage.delete(key)
                    }}
                  />
                </div>
              ))}
            </div>

            <Separator orientation="horizontal" className="my-3" />

            <Text variant="subtitle" className="mb-2">
              {getText('paywallDevtoolsPaywallFeaturesToggles')}
            </Text>

            <div className="flex flex-col gap-1">
              {Object.entries(features).map(([feature, configuration]) => {
                // eslint-disable-next-line no-restricted-syntax
                const featureName = feature as billing.PaywallFeatureName
                const { label, descriptionTextId } = getFeature(featureName)

                return (
                  <div key={feature} className="flex flex-col">
                    <Switch
                      className="group flex items-center gap-1"
                      isSelected={configuration.isForceEnabled ?? true}
                      onChange={(value) => {
                        onConfigurationChange(featureName, {
                          isForceEnabled: value,
                        })
                      }}
                    >
                      <div className="box-border flex h-4 w-[28px] shrink-0 cursor-default items-center rounded-full bg-primary/30 bg-clip-padding p-0.5 shadow-inner outline-none ring-black transition duration-200 ease-in-out group-focus-visible:ring-2 group-pressed:bg-primary/60 group-selected:bg-primary group-selected:group-pressed:bg-primary/50">
                        <span className="aspect-square h-full flex-none translate-x-0 transform rounded-full bg-white transition duration-200 ease-in-out group-selected:translate-x-[100%]" />
                      </div>

                      <Text className="flex-1">{getText(label)}</Text>
                    </Switch>

                    <Text variant="body" color="disabled">
                      {getText(descriptionTextId)}
                    </Text>
                  </div>
                )
              })}
            </div>
          </Popover>
        </DialogTrigger>
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
