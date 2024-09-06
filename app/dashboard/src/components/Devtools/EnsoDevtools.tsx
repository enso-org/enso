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
import * as textProvider from '#/providers/TextProvider'
import {
  useEnableVersionChecker,
  usePaywallDevtools,
  useSetEnableVersionChecker,
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
  useSetFeatureFlags,
} from '#/providers/FeatureFlagsProvider'
import { useLocalStorage } from '#/providers/LocalStorageProvider'
import * as backend from '#/services/Backend'
import LocalStorage from '#/utilities/LocalStorage'
import { unsafeEntries } from 'enso-common/src/utilities/data/object'

/**
 * A component that provides a UI for toggling paywall features.
 */
export function EnsoDevtools() {
  const { getText } = textProvider.useText()
  const { authQueryKey, session } = authProvider.useAuth()
  const queryClient = reactQuery.useQueryClient()
  const { getFeature } = billing.usePaywallFeatures()
  const { features, setFeature } = usePaywallDevtools()
  const enableVersionChecker = useEnableVersionChecker()
  const setEnableVersionChecker = useSetEnableVersionChecker()
  const { localStorage } = useLocalStorage()

  const featureFlags = useFeatureFlags()
  const setFeatureFlags = useSetFeatureFlags()

  return (
    <Portal>
      <ariaComponents.DialogTrigger>
        <ariaComponents.Button
          icon={DevtoolsLogo}
          aria-label={getText('ensoDevtoolsButtonLabel')}
          variant="icon"
          rounded="full"
          size="hero"
          className="fixed bottom-16 right-3 z-50"
          data-ignore-click-outside
        />

        <Popover>
          <Text.Heading disableLineHeightCompensation>
            {getText('ensoDevtoolsPopoverHeading')}
          </Text.Heading>

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

              {/* eslint-disable-next-line no-restricted-syntax */}
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
            schema={(z) => z.object({ enableVersionChecker: z.boolean() })}
            defaultValues={{ enableVersionChecker: enableVersionChecker ?? !IS_DEV_MODE }}
          >
            {({ form }) => (
              <ariaComponents.Switch
                form={form}
                name="enableVersionChecker"
                label={getText('enableVersionChecker')}
                description={getText('enableVersionCheckerDescription')}
                onChange={(value) => {
                  setEnableVersionChecker(value)
                }}
              />
            )}
          </ariaComponents.Form>

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

          <ariaComponents.Separator orientation="horizontal" className="my-3" />

          <ariaComponents.Text variant="subtitle" className="mb-2">
            {getText('ensoDevtoolsFeatureFlags')}

            <ariaComponents.Form
              gap="small"
              schema={FEATURE_FLAGS_SCHEMA}
              formOptions={{ mode: 'onChange' }}
              defaultValues={{
                enableMultitabs: featureFlags.enableMultitabs,
                enableAssetsTableBackgroundRefresh: featureFlags.enableAssetsTableBackgroundRefresh,
                assetsTableBackgroundRefreshInterval:
                  featureFlags.assetsTableBackgroundRefreshInterval,
              }}
            >
              {(form) => (
                <>
                  <ariaComponents.Switch
                    form={form}
                    name="enableMultitabs"
                    label={getText('enableMultitabs')}
                    description={getText('enableMultitabsDescription')}
                    onChange={(value) => {
                      setFeatureFlags('enableMultitabs', value)
                    }}
                  />

                  <div>
                    <ariaComponents.Switch
                      form={form}
                      name="enableAssetsTableBackgroundRefresh"
                      label={getText('enableAssetsTableBackgroundRefresh')}
                      description={getText('enableAssetsTableBackgroundRefreshDescription')}
                      onChange={(value) => {
                        setFeatureFlags('enableAssetsTableBackgroundRefresh', value)
                      }}
                    />
                    <ariaComponents.Input
                      form={form}
                      type="number"
                      inputMode="numeric"
                      name="assetsTableBackgroundRefreshInterval"
                      label={getText('enableAssetsTableBackgroundRefreshInterval')}
                      description={getText('enableAssetsTableBackgroundRefreshIntervalDescription')}
                      onChange={(event) => {
                        setFeatureFlags(
                          'assetsTableBackgroundRefreshInterval',
                          event.target.valueAsNumber,
                        )
                      }}
                    />
                  </div>
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
        </Popover>
      </ariaComponents.DialogTrigger>
    </Portal>
  )
}
