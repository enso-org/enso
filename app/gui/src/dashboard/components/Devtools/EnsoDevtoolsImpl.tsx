/** @file A list of toggles for paywall features. */
import CrossIcon from '#/assets/cross.svg'
import { Button, CopyButton, type ButtonProps } from '#/components/Button'
import {
  useEnableVersionChecker,
  usePaywallDevtools,
  useSetEnableVersionChecker,
  useShowEnsoDevtools,
  useToggleEnsoDevtools,
} from '#/components/Devtools/EnsoDevtoolsProvider'
import { Dialog, Popover, POPOVER_STYLES } from '#/components/Dialog'
import { Form } from '#/components/Form'
import { Icon } from '#/components/Icon'
import { Input } from '#/components/Inputs/Input'
import { Radio } from '#/components/Radio'
import { Separator } from '#/components/Separator'
import { Switch } from '#/components/Switch'
import { Text } from '#/components/Text'
import { Tooltip } from '#/components/Tooltip'
import { Underlay } from '#/components/Underlay'
import { VisualTooltip } from '#/components/VisualTooltip'
import { usePaywall, usePaywallFeatures } from '#/hooks/billing'
import LocalStorage, { useLocalStorageValues } from '#/utilities/LocalStorage'
import { safeJsonParse } from '#/utilities/safeJsonParse'
import {
  DEFAULT_ASSETS_TABLE_REFRESH_INTERVAL_MS,
  DEFAULT_FILE_CHUNK_UPLOAD_POOL_SIZE,
  DEFAULT_GET_LOG_EVENTS_PAGE_SIZE,
  DEFAULT_LIST_DIRECTORY_PAGE_SIZE,
  FEATURE_FLAGS_SCHEMA,
} from '$/providers/featureFlags'
import { useLocalStorage, useText } from '$/providers/react'
import { useUserSession } from '$/providers/react/auth'
import { useFeatureFlags, useSetFeatureFlag } from '$/providers/react/featureFlags'
import { useQueryClient } from '@tanstack/react-query'
import * as backend from 'enso-common/src/services/Backend'
import { unsafeKeys } from 'enso-common/src/utilities/data/object'
import { IS_DEV_MODE } from 'enso-common/src/utilities/detect'
import { toast } from 'react-toastify'
import { twJoin } from 'tailwind-merge'
import invariant from 'tiny-invariant'

/** Props for a {@link DeveloperOverrideEntry}. */
interface DeveloperOverrideEntryProps {
  readonly reset: ButtonProps['onPress']
  readonly children: string
}

/** An entry in {@link EnsoDevStatus}. */
function DeveloperOverrideEntry(props: DeveloperOverrideEntryProps) {
  const { reset, children } = props

  const { getText } = useText()

  return (
    <div className="flex items-center gap-2">
      <Button
        variant="icon"
        icon={CrossIcon}
        aria-label={getText('reset')}
        tooltipPlacement="right"
        onPress={reset}
      />
      <Text>{children}</Text>
    </div>
  )
}

/** A display of current developer overrides. */
export function EnsoDevStatus() {
  const { getText } = useText()
  const showEnsoDevtools = useShowEnsoDevtools()
  const versionCheckerEnabled = useEnableVersionChecker() ?? false
  const setVersionCheckerEnabled = useSetEnableVersionChecker()
  const {
    developerPlanOverride,
    showDeveloperIds,
    enableMultitabs,
    enableAssetsTableBackgroundRefresh,
    assetsTableBackgroundRefreshInterval,
    enableCloudExecution,
    enableAdvancedProjectExecutionOptions,
    listDirectoryPageSize,
    getLogEventsPageSize,
    fileChunkUploadPoolSize,
    unsafeDarkTheme,
  } = useFeatureFlags()
  const setFeatureFlag = useSetFeatureFlag()

  const planName = (() => {
    switch (developerPlanOverride) {
      case backend.Plan.free:
      case backend.Plan.solo:
      case backend.Plan.team:
      case backend.Plan.enterprise: {
        return getText(developerPlanOverride)
      }
      case undefined: {
        return
      }
    }
  })()

  const isOverridden =
    planName != null ||
    versionCheckerEnabled ||
    !enableAssetsTableBackgroundRefresh ||
    assetsTableBackgroundRefreshInterval !== DEFAULT_ASSETS_TABLE_REFRESH_INTERVAL_MS ||
    !enableCloudExecution ||
    showDeveloperIds ||
    enableMultitabs ||
    enableAdvancedProjectExecutionOptions ||
    listDirectoryPageSize !== DEFAULT_LIST_DIRECTORY_PAGE_SIZE ||
    getLogEventsPageSize !== DEFAULT_GET_LOG_EVENTS_PAGE_SIZE ||
    fileChunkUploadPoolSize !== DEFAULT_FILE_CHUNK_UPLOAD_POOL_SIZE ||
    unsafeDarkTheme
  if (!isOverridden) return null

  const styles = POPOVER_STYLES({ size: 'auto-xxsmall' })

  return (
    <div
      className={styles.base({
        className: twJoin('absolute left-3', showEnsoDevtools ? 'bottom-[4.25rem]' : 'bottom-3'),
      })}
    >
      <div className={styles.dialog()}>
        {planName != null && (
          <DeveloperOverrideEntry
            reset={() => {
              setFeatureFlag('developerPlanOverride', undefined)
            }}
          >
            {getText('planOverriddenToX', planName)}
          </DeveloperOverrideEntry>
        )}
        {versionCheckerEnabled && (
          <DeveloperOverrideEntry
            reset={() => {
              setVersionCheckerEnabled(false)
            }}
          >
            {getText('versionCheckerEnabled')}
          </DeveloperOverrideEntry>
        )}
        {!enableAssetsTableBackgroundRefresh && (
          <DeveloperOverrideEntry
            reset={() => {
              setFeatureFlag('enableAssetsTableBackgroundRefresh', true)
            }}
          >
            {getText('assetsTableBackgroundRefreshDisabled')}
          </DeveloperOverrideEntry>
        )}
        {assetsTableBackgroundRefreshInterval !== DEFAULT_ASSETS_TABLE_REFRESH_INTERVAL_MS && (
          <DeveloperOverrideEntry
            reset={() => {
              setFeatureFlag(
                'assetsTableBackgroundRefreshInterval',
                DEFAULT_ASSETS_TABLE_REFRESH_INTERVAL_MS,
              )
            }}
          >
            {getText(
              'assetsTableBackgroundRefreshIntervalOverriddenToXMs',
              assetsTableBackgroundRefreshInterval,
            )}
          </DeveloperOverrideEntry>
        )}
        {!enableCloudExecution && (
          <DeveloperOverrideEntry
            reset={() => {
              setFeatureFlag('enableCloudExecution', true)
            }}
          >
            {getText('cloudExecutionDisabled')}
          </DeveloperOverrideEntry>
        )}
        {showDeveloperIds && (
          <DeveloperOverrideEntry
            reset={() => {
              setFeatureFlag('showDeveloperIds', false)
            }}
          >
            {getText('showingDeveloperIds')}
          </DeveloperOverrideEntry>
        )}
        {enableMultitabs && (
          <DeveloperOverrideEntry
            reset={() => {
              setFeatureFlag('enableMultitabs', false)
            }}
          >
            {getText('multitabsEnabled')}
          </DeveloperOverrideEntry>
        )}
        {enableAdvancedProjectExecutionOptions && (
          <DeveloperOverrideEntry
            reset={() => {
              setFeatureFlag('enableAdvancedProjectExecutionOptions', false)
            }}
          >
            {getText('advancedProjectExecutionOptionsEnabled')}
          </DeveloperOverrideEntry>
        )}
        {listDirectoryPageSize !== DEFAULT_LIST_DIRECTORY_PAGE_SIZE && (
          <DeveloperOverrideEntry
            reset={() => {
              setFeatureFlag('listDirectoryPageSize', DEFAULT_LIST_DIRECTORY_PAGE_SIZE)
            }}
          >
            {getText('willFetchUpToXAssetsPerPage', listDirectoryPageSize)}
          </DeveloperOverrideEntry>
        )}
        {getLogEventsPageSize !== DEFAULT_GET_LOG_EVENTS_PAGE_SIZE && (
          <DeveloperOverrideEntry
            reset={() => {
              setFeatureFlag('getLogEventsPageSize', DEFAULT_GET_LOG_EVENTS_PAGE_SIZE)
            }}
          >
            {getText('willFetchUpToXLogEntriesPerPage', getLogEventsPageSize)}
          </DeveloperOverrideEntry>
        )}
        {fileChunkUploadPoolSize !== DEFAULT_FILE_CHUNK_UPLOAD_POOL_SIZE && (
          <DeveloperOverrideEntry
            reset={() => {
              setFeatureFlag('fileChunkUploadPoolSize', DEFAULT_FILE_CHUNK_UPLOAD_POOL_SIZE)
            }}
          >
            {getText('willUploadUpToXFileChunksAtOnce', fileChunkUploadPoolSize)}
          </DeveloperOverrideEntry>
        )}
        {unsafeDarkTheme && (
          <DeveloperOverrideEntry
            reset={() => {
              setFeatureFlag('unsafeDarkTheme', false)
            }}
          >
            {getText('developerDarkThemeEnabled')}
          </DeveloperOverrideEntry>
        )}
      </div>
    </div>
  )
}

/** A UI for toggling paywall features. */
export function EnsoDevtools() {
  const { getText } = useText()

  const queryClient = useQueryClient()
  const session = useUserSession()

  const { getFeature } = usePaywallFeatures()
  const toggleEnsoDevtools = useToggleEnsoDevtools()

  const { features, setFeature } = usePaywallDevtools()

  const currentlyViewedPlan = session?.user.plan ?? backend.Plan.free
  const { isFeatureUnderPaywall } = usePaywall({ plan: currentlyViewedPlan })

  const enableVersionChecker = useEnableVersionChecker()
  const setEnableVersionChecker = useSetEnableVersionChecker()

  const localStorage = useLocalStorage()
  const localStorageState = useLocalStorageValues(localStorage)

  const featureFlags = useFeatureFlags()
  const setFeatureFlag = useSetFeatureFlag()

  return (
    <Popover.Trigger>
      <Underlay className="fixed bottom-3 left-3 z-50 rounded-full">
        <Button
          icon="enso_logo"
          aria-label={getText('ensoDevtoolsButtonLabel')}
          variant="icon"
          rounded="full"
          size="hero"
          data-ignore-click-outside
        />
      </Underlay>

      <Popover shouldCloseOnInteractOutside={() => true}>
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

        <Button
          variant="outline"
          onPress={async () => {
            await queryClient.clearWithPersister()
            location.reload()
          }}
        >
          {getText('clearCacheAndReload')}
        </Button>

        <Separator orientation="horizontal" className="my-3" />

        {session != null && (
          <>
            <Text variant="subtitle">{getText('ensoDevtoolsPlanSelectSubtitle')}</Text>

            <Form
              gap="small"
              schema={(schema) => schema.object({ plan: schema.nativeEnum(backend.Plan) })}
              defaultValues={{ plan: session.user.plan }}
            >
              <Radio.Group
                name="plan"
                onChange={(value) => {
                  invariant(backend.isPlan(value), 'Invalid plan type')
                  setFeatureFlag('developerPlanOverride', value)
                }}
              >
                <Radio label={getText('free')} value={backend.Plan.free} />
                <Radio label={getText('solo')} value={backend.Plan.solo} />
                <Radio label={getText('team')} value={backend.Plan.team} />
                <Radio label={getText('enterprise')} value={backend.Plan.enterprise} />
              </Radio.Group>

              <Button
                size="small"
                variant="outline"
                onPress={() => {
                  setFeatureFlag('developerPlanOverride', undefined)
                }}
              >
                {getText('reset')}
              </Button>
            </Form>

            <Separator orientation="horizontal" className="my-3" />
          </>
        )}

        <Text variant="subtitle" className="mb-2">
          {getText('productionOnlyFeatures')}
        </Text>

        <Form
          schema={(schema) => schema.object({ enableVersionChecker: schema.boolean() })}
          defaultValues={{ enableVersionChecker: enableVersionChecker ?? !IS_DEV_MODE }}
        >
          {({ form }) => (
            <Switch
              form={form}
              name="enableVersionChecker"
              label={getText('enableVersionChecker')}
              description={getText('enableVersionCheckerDescription')}
              onChange={(value) => {
                setEnableVersionChecker(value)
              }}
            />
          )}
        </Form>

        <Separator orientation="horizontal" className="my-3" />

        <Text variant="subtitle" className="mb-2">
          {getText('ensoDevtoolsFeatureFlags')}

          <Form
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
                <Switch
                  form={form}
                  name="debugHoverAreas"
                  label={'Debug hover areas'}
                  description={'Make all mouse hoverable areas visible on the graph.'}
                  onChange={(value) => {
                    setFeatureFlag('debugHoverAreas', value)
                  }}
                />
                <Switch
                  form={form}
                  name="showDeveloperIds"
                  label={getText('ensoDevtoolsFeatureFlags.showDeveloperIds')}
                  description={getText('ensoDevtoolsFeatureFlags.showDeveloperIdsDescription')}
                  onChange={(value) => {
                    setFeatureFlag('showDeveloperIds', value)
                  }}
                />
                <Switch
                  form={form}
                  name="enableMultitabs"
                  label={getText('ensoDevtoolsFeatureFlags.enableMultitabs')}
                  description={getText('ensoDevtoolsFeatureFlags.enableMultitabsDescription')}
                  onChange={(value) => {
                    setFeatureFlag('enableMultitabs', value)
                  }}
                />
                <div>
                  <Switch
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
                  <Input
                    form={form}
                    type="number"
                    inputMode="numeric"
                    name="assetsTableBackgroundRefreshInterval"
                    label={getText('ensoDevtoolsFeatureFlags.assetsTableBackgroundRefreshInterval')}
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
                  <Switch
                    form={form}
                    name="unsafeDarkTheme"
                    label="Developer Dark Theme"
                    description="Enable quick-and-dirty dark theme for developer use only"
                    onChange={(value) => {
                      setFeatureFlag('unsafeDarkTheme', value)
                    }}
                  />
                </div>
                <Switch
                  form={form}
                  name="enableCloudExecution"
                  label="Enable Cloud Execution"
                  description="Enable Cloud Execution"
                  onChange={(value) => {
                    setFeatureFlag('enableCloudExecution', value)
                  }}
                />
                <Switch
                  form={form}
                  name="enableAdvancedProjectExecutionOptions"
                  label="Enable Advanced Project Excecution Options"
                  description="Enable Advanced Project Excecution Options"
                  onChange={(value) => {
                    setFeatureFlag('enableAdvancedProjectExecutionOptions', value)
                  }}
                />
                <Input
                  form={form}
                  type="number"
                  inputMode="numeric"
                  name="listDirectoryPageSize"
                  label={getText('ensoDevtoolsFeatureFlags.listDirectoryPageSize')}
                  description={getText('ensoDevtoolsFeatureFlags.listDirectoryPageSizeDescription')}
                  onChange={(event) => {
                    setFeatureFlag('listDirectoryPageSize', event.target.valueAsNumber)
                  }}
                />
                <Input
                  form={form}
                  type="number"
                  inputMode="numeric"
                  name="getLogEventsPageSize"
                  label={getText('ensoDevtoolsFeatureFlags.getLogEventsPageSize')}
                  description={getText('ensoDevtoolsFeatureFlags.getLogEventsPageSizeDescription')}
                  onChange={(event) => {
                    setFeatureFlag('getLogEventsPageSize', event.target.valueAsNumber)
                  }}
                />
                <Input
                  form={form}
                  type="number"
                  inputMode="numeric"
                  name="fileChunkUploadPoolSize"
                  label={getText('ensoDevtoolsFeatureFlags.fileChunkUploadPoolSize')}
                  description={getText(
                    'ensoDevtoolsFeatureFlags.fileChunkUploadPoolSizeDescription',
                  )}
                  onChange={(event) => {
                    setFeatureFlag('fileChunkUploadPoolSize', event.target.valueAsNumber)
                  }}
                />
              </>
            )}
          </Form>
        </Text>

        <Separator orientation="horizontal" className="my-3" />

        <Text variant="subtitle" className="mb-2">
          {getText('ensoDevtoolsPaywallFeaturesToggles')}
        </Text>

        <Form
          gap="small"
          schema={(schema) =>
            schema.object(
              Object.fromEntries(unsafeKeys(features).map((key) => [key, schema.boolean()])),
            )
          }
          defaultValues={Object.fromEntries(
            unsafeKeys(features).map((featureName) => {
              return [featureName, features[featureName].isForceEnabled ?? false]
            }),
          )}
        >
          {unsafeKeys(features).map((featureName) => {
            const { label, descriptionTextId } = getFeature(featureName)
            return (
              <Switch
                key={featureName}
                name={featureName}
                label={getText(label)}
                halfway={!isFeatureUnderPaywall(featureName, true)}
                description={getText(descriptionTextId)}
                onChange={(value) => {
                  setFeature(featureName, value || null)
                }}
              />
            )
          })}
        </Form>

        <Separator orientation="horizontal" className="my-3" />

        <div className="mb-2 flex w-full items-center justify-between gap-3">
          <Text variant="subtitle">{getText('localStorage')}</Text>

          <Tooltip.Trigger>
            <CopyButton className="ml-auto" copyText={JSON.stringify(localStorageState, null, 2)} />

            <Tooltip>
              <Text>Copy everything to clipboard</Text>
            </Tooltip>
          </Tooltip.Trigger>

          <Tooltip.Trigger>
            <Button
              variant="icon"
              size="small"
              icon="paste"
              onPress={async () => {
                const text = await window.navigator.clipboard.readText()
                localStorage.setManyFromUntrustedSource(safeJsonParse(text, null))
                toast.success('State pasted')
              }}
            />

            <Tooltip>Paste state from clipboard</Tooltip>
          </Tooltip.Trigger>

          <Button
            aria-label={getText('deleteAll')}
            size="small"
            variant="icon"
            icon="trash"
            onPress={() => {
              localStorage.clearAll()
            }}
          />
        </div>

        <div className="flex flex-col gap-1.5">
          {LocalStorage.getAllRegisteredKeys().map((key) => {
            const metadata = LocalStorage.getKeyMetadata(key)
            const title = key
              .replace(/[A-Z]/g, (m) => ' ' + m.toLowerCase())
              .replace(/^./, (m) => m.toUpperCase())

            return (
              <div key={key} className="flex w-full items-center justify-between gap-1">
                <div className="flex items-center gap-1">
                  <Text variant="body">{title}</Text>

                  {metadata.isUserSpecific === true && (
                    <VisualTooltip tooltip="User specific storage item">
                      <Icon icon="default_user" size="small" color="primary" />
                    </VisualTooltip>
                  )}
                </div>

                <Button.Group
                  align="end"
                  buttonVariants={{ size: 'small', variant: 'icon', extraClickZone: 'small' }}
                >
                  <Dialog.Trigger>
                    <Button aria-label="Edit" icon="edit" />

                    <Dialog title={`Edit ${title}`}>
                      <Form
                        method="dialog"
                        schema={(schema) =>
                          schema.object({
                            value: schema
                              .any()
                              .transform((value) => {
                                if (typeof value === 'string') {
                                  return safeJsonParse(value, null)
                                }

                                // eslint-disable-next-line @typescript-eslint/no-unsafe-return
                                return value
                              })
                              .refine(
                                (value) => metadata.schema.safeParse(value).success,
                                'Invalid JSON or value does not match schema',
                              )
                              // Piping the value only for type inference.
                              .pipe(metadata.schema),
                          })
                        }
                        defaultValues={{
                          value: JSON.stringify(localStorageState[key], null, 2),
                        }}
                        onSubmit={(data) => {
                          localStorage.set(key, data.value)
                        }}
                      >
                        <Input
                          name="value"
                          label="Enter valid JSON"
                          addonStart={<Icon icon="braces" />}
                        />

                        <Form.Submit />

                        <Form.FormError />
                      </Form>
                    </Dialog>
                  </Dialog.Trigger>

                  <Button
                    isDisabled={localStorageState[key] == null}
                    aria-label={getText('delete')}
                    icon={CrossIcon}
                    onPress={() => {
                      localStorage.delete(key)
                    }}
                  />
                </Button.Group>
              </div>
            )
          })}
        </div>
      </Popover>
    </Popover.Trigger>
  )
}
