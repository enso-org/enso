/**
 * @file
 *
 * A component that provides a UI for toggling paywall features.
 */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import { IS_DEV_MODE } from 'enso-common/src/detect'

import CrossIcon from '#/assets/cross.svg'
import DevtoolsLogo from '#/assets/enso_logo.svg'
import TrashIcon from '#/assets/trash.svg'

import { SETUP_PATH } from '#/appUtils'

import * as billing from '#/hooks/billing'

import * as authProvider from '#/providers/AuthProvider'
import { UserSessionType } from '#/providers/AuthProvider'
import * as textProvider from '#/providers/TextProvider'
import {
  useAnimationsDisabled,
  useEnableVersionChecker,
  usePaywallDevtools,
  useSetAnimationsDisabled,
  useSetEnableVersionChecker,
  useToggleEnsoDevtools,
} from './EnsoDevtoolsProvider'

import * as ariaComponents from '#/components/AriaComponents'
import Portal from '#/components/Portal'

import {
  Button,
  Form,
  Popover,
  Radio,
  RadioGroup,
  Separator,
  Text,
} from '#/components/AriaComponents'
import {
  FEATURE_FLAGS_SCHEMA,
  useFeatureFlags,
  useSetFeatureFlag,
} from '#/providers/FeatureFlagsProvider'
import { useLocalStorage } from '#/providers/LocalStorageProvider'
import * as backend from '#/services/Backend'
import LocalStorage, { type LocalStorageData } from '#/utilities/LocalStorage'
import { unsafeKeys } from '#/utilities/object'

/** A component that provides a UI for toggling paywall features. */
export function EnsoDevtools() {
  const { getText } = textProvider.useText()

  const { authQueryKey, session } = authProvider.useAuth()
  const queryClient = reactQuery.useQueryClient()
  const { getFeature } = billing.usePaywallFeatures()
  const toggleEnsoDevtools = useToggleEnsoDevtools()

  const { features, setFeature } = usePaywallDevtools()
  const enableVersionChecker = useEnableVersionChecker()
  const setEnableVersionChecker = useSetEnableVersionChecker()

  const animationsDisabled = useAnimationsDisabled()
  const setAnimationsDisabled = useSetAnimationsDisabled()

  const { localStorage } = useLocalStorage()
  const [localStorageState, setLocalStorageState] = React.useState<Partial<LocalStorageData>>({})

  // Re-render when localStorage changes.
  React.useEffect(() => localStorage.subscribeAll(setLocalStorageState), [localStorage])

  const featureFlags = useFeatureFlags()
  const setFeatureFlag = useSetFeatureFlag()

  return (
    <Portal>
      <ariaComponents.DialogTrigger>
        <ariaComponents.Underlay className="fixed bottom-3 left-3 z-50 rounded-full">
          <ariaComponents.Button
            icon={DevtoolsLogo}
            aria-label={getText('ensoDevtoolsButtonLabel')}
            variant="icon"
            rounded="full"
            size="hero"
            data-ignore-click-outside
          />
        </ariaComponents.Underlay>

        <Popover>
          <div className="flex items-center justify-between">
            <Text.Heading disableLineHeightCompensation>
              {getText('ensoDevtoolsPopoverHeading')}
            </Text.Heading>

            <Button
              variant="icon"
              onPress={() => {
                toggleEnsoDevtools()
              }}
            >
              {getText('hideDevtools')}
            </Button>
          </div>

          <Separator orientation="horizontal" className="my-3" />

          {session?.type === UserSessionType.full && (
            <>
              <Text variant="subtitle">{getText('ensoDevtoolsPlanSelectSubtitle')}</Text>

              <Form
                gap="small"
                schema={(schema) => schema.object({ plan: schema.nativeEnum(backend.Plan) })}
                defaultValues={{ plan: session.user.plan ?? backend.Plan.free }}
              >
                {({ form }) => (
                  <>
                    <RadioGroup
                      name="plan"
                      onChange={(value) => {
                        queryClient.setQueryData(authQueryKey, {
                          ...session,
                          user: { ...session.user, plan: value },
                        })
                      }}
                    >
                      <Radio label={getText('free')} value={backend.Plan.free} />
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

              <Button variant="link" href={SETUP_PATH + '?__qd-debg__=true'}>
                Open setup page
              </Button>

              <Separator orientation="horizontal" className="my-3" />
            </>
          )}

          <ariaComponents.Text variant="subtitle" className="mb-2">
            {getText('productionOnlyFeatures')}
          </ariaComponents.Text>

          <ariaComponents.Form
            schema={(z) =>
              z.object({ enableVersionChecker: z.boolean(), disableAnimations: z.boolean() })
            }
            defaultValues={{
              enableVersionChecker: enableVersionChecker ?? !IS_DEV_MODE,
              disableAnimations: animationsDisabled,
            }}
          >
            {({ form }) => (
              <>
                <ariaComponents.Switch
                  form={form}
                  name="disableAnimations"
                  label={getText('disableAnimations')}
                  description={getText('disableAnimationsDescription')}
                  onChange={(value) => {
                    setAnimationsDisabled(value)
                  }}
                />

                <ariaComponents.Switch
                  form={form}
                  name="enableVersionChecker"
                  label={getText('enableVersionChecker')}
                  description={getText('enableVersionCheckerDescription')}
                  onChange={(value) => {
                    setEnableVersionChecker(value)
                  }}
                />
              </>
            )}
          </ariaComponents.Form>

          <ariaComponents.Separator orientation="horizontal" className="my-3" />

          <ariaComponents.Text variant="subtitle" className="mb-2">
            {getText('ensoDevtoolsFeatureFlags')}

            <ariaComponents.Form
              gap="small"
              schema={FEATURE_FLAGS_SCHEMA}
              formOptions={{ mode: 'onChange' }}
              defaultValues={Object.fromEntries(
                // FEATURE_FLAGS_SCHEMA is statically known, so we can safely cast to keyof FeatureFlags.
                unsafeKeys(FEATURE_FLAGS_SCHEMA.shape).map((key) => [key, featureFlags[key]]),
              )}
            >
              {(form) => (
                <>
                  <ariaComponents.Switch
                    form={form}
                    name="enableMultitabs"
                    label={getText('ensoDevtoolsFeatureFlags.enableMultitabs')}
                    description={getText('ensoDevtoolsFeatureFlags.enableMultitabsDescription')}
                    onChange={(value) => {
                      setFeatureFlag('enableMultitabs', value)
                    }}
                  />

                  <div>
                    <ariaComponents.Switch
                      form={form}
                      name="enableAssetsTableBackgroundRefresh"
                      label={getText('ensoDevtoolsFeatureFlags.enableAssetsTableBackgroundRefresh')}
                      description={getText(
                        'ensoDevtoolsFeatureFlags.enableAssetsTableBackgroundRefreshDescription',
                      )}
                      onChange={(value) => {
                        setFeatureFlag('enableAssetsTableBackgroundRefresh', value)
                      }}
                    />
                    <ariaComponents.Input
                      form={form}
                      type="number"
                      inputMode="numeric"
                      name="assetsTableBackgroundRefreshInterval"
                      label={getText(
                        'ensoDevtoolsFeatureFlags.assetsTableBackgroundRefreshInterval',
                      )}
                      description={getText(
                        'ensoDevtoolsFeatureFlags.assetsTableBackgroundRefreshIntervalDescription',
                      )}
                      onChange={(event) => {
                        setFeatureFlag(
                          'assetsTableBackgroundRefreshInterval',
                          event.target.valueAsNumber,
                        )
                      }}
                    />
                  </div>

                  <ariaComponents.Switch
                    form={form}
                    name="enableCloudExecution"
                    label="Enable Cloud Execution"
                    description="Enable Cloud Execution"
                    onChange={(value) => {
                      setFeatureFlag('enableCloudExecution', value)
                    }}
                  />

                  <ariaComponents.Switch
                    form={form}
                    name="enableAsyncExecution"
                    label="Enable Async Execution"
                    description="Enable Async Execution"
                    onChange={(value) => {
                      setFeatureFlag('enableAsyncExecution', value)
                    }}
                  />
                </>
              )}
            </ariaComponents.Form>
          </ariaComponents.Text>

          <ariaComponents.Separator orientation="horizontal" className="my-3" />

          <ariaComponents.Text variant="subtitle" className="mb-2">
            {getText('ensoDevtoolsPaywallFeaturesToggles')}
          </ariaComponents.Text>

          <ariaComponents.Form
            gap="small"
            schema={(z) =>
              z.object(Object.fromEntries(Object.keys(features).map((key) => [key, z.boolean()])))
            }
            defaultValues={Object.fromEntries(
              Object.keys(features).map((feature) => {
                // eslint-disable-next-line no-restricted-syntax
                const featureName = feature as billing.PaywallFeatureName
                return [featureName, features[featureName].isForceEnabled ?? true]
              }),
            )}
          >
            {Object.keys(features).map((feature) => {
              // eslint-disable-next-line no-restricted-syntax
              const featureName = feature as billing.PaywallFeatureName
              const { label, descriptionTextId } = getFeature(featureName)

              return (
                <ariaComponents.Switch
                  key={feature}
                  name={featureName}
                  label={getText(label)}
                  description={getText(descriptionTextId)}
                  onChange={(value) => {
                    setFeature(featureName, value)
                  }}
                />
              )
            })}
          </ariaComponents.Form>

          <Separator orientation="horizontal" className="my-3" />

          <div className="mb-2 flex w-full items-center justify-between">
            <Text variant="subtitle">{getText('localStorage')}</Text>

            <Button
              aria-label={getText('deleteAll')}
              size="small"
              variant="icon"
              icon={TrashIcon}
              onPress={() => {
                for (const key of LocalStorage.getAllKeys()) {
                  localStorage.delete(key)
                }
              }}
            />
          </div>

          <div className="flex flex-col gap-0.5">
            {LocalStorage.getAllKeys().map((key) => (
              <div key={key} className="flex w-full items-center justify-between gap-1">
                <Text variant="body">
                  {key
                    .replace(/[A-Z]/g, (m) => ' ' + m.toLowerCase())
                    .replace(/^./, (m) => m.toUpperCase())}
                </Text>

                <Button
                  variant="icon"
                  size="small"
                  isDisabled={localStorageState[key] == null}
                  aria-label={getText('delete')}
                  icon={CrossIcon}
                  onPress={() => {
                    localStorage.delete(key)
                  }}
                />
              </div>
            ))}
          </div>
        </Popover>
      </ariaComponents.DialogTrigger>
    </Portal>
  )
}
