/** @file Modal for accepting the terms of service. */
import { Button } from '#/components/Button'
import { Checkbox } from '#/components/Checkbox'
import { Dialog } from '#/components/Dialog'
import { Form } from '#/components/Form'
import { Text } from '#/components/Text'
import LocalStorage from '#/utilities/LocalStorage'
import { useText } from '$/providers/react'
import * as vueQuery from '@tanstack/vue-query'
import { memo } from 'react'
import { computed, effectScope, onScopeDispose, proxyRefs } from 'vue'
import * as z from 'zod'
declare module '#/utilities/LocalStorage' {
  /** Metadata containing the version hash of the terms of service that the user has accepted. */
  interface LocalStorageData {
    readonly termsOfService: z.infer<typeof TOS_SCHEMA>
    readonly privacyPolicy: z.infer<typeof PRIVACY_POLICY_SCHEMA>
  }
}

const TEN_MINUTES_MS = 600_000
const TOS_SCHEMA = z.object({ versionHash: z.string() })
const PRIVACY_POLICY_SCHEMA = z.object({ versionHash: z.string() })
const TOS_ENDPOINT_SCHEMA = z.object({ hash: z.string() })
const PRIVACY_POLICY_ENDPOINT_SCHEMA = z.object({ hash: z.string() })

LocalStorage.registerKey('termsOfService', { schema: TOS_SCHEMA })
LocalStorage.registerKey('privacyPolicy', { schema: PRIVACY_POLICY_SCHEMA })

// eslint-disable-next-line react-refresh/only-export-components
export const latestTermsOfServiceQueryOptions = vueQuery.queryOptions({
  queryKey: ['termsOfService', 'currentVersion'],
  queryFn: async () => {
    const response = await fetch(new URL('/eula.json', $config.ENSO_HOST))
    if (!response.ok) {
      throw new Error('Failed to fetch Terms of Service')
    } else {
      return TOS_ENDPOINT_SCHEMA.parse(await response.json())
    }
  },
  select: (data) => data.hash,
  refetchOnWindowFocus: true,
  refetchIntervalInBackground: true,
  refetchInterval: TEN_MINUTES_MS,
})

// eslint-disable-next-line react-refresh/only-export-components
export const latestPrivacyPolicyQueryOptions = vueQuery.queryOptions({
  queryKey: ['privacyPolicy', 'currentVersion'],
  queryFn: async () => {
    const response = await fetch(new URL('/privacy.json', $config.ENSO_HOST))
    if (!response.ok) {
      throw new Error('Failed to fetch Privacy Policy')
    } else {
      return PRIVACY_POLICY_ENDPOINT_SCHEMA.parse(await response.json())
    }
  },
  select: (data) => data.hash,
  refetchOnWindowFocus: true,
  refetchIntervalInBackground: true,
  refetchInterval: TEN_MINUTES_MS,
})

export async function useAgreementsModalProps(queryClient: vueQuery.QueryClient) {
  const localStorage = LocalStorage.getInstance()
  const cachedTosHash = computed(() => localStorage.get('termsOfService'))
  const cachedPrivacyPolicyHash = computed(() => localStorage.get('privacyPolicy'))

  const scope = effectScope()
  const initialTosHash =
    cachedTosHash.value?.versionHash ??
    (await queryClient.fetchQuery(latestTermsOfServiceQueryOptions)).hash
  const initialPrivacyPolicyHash =
    cachedPrivacyPolicyHash.value?.versionHash ??
    (await queryClient.fetchQuery(latestPrivacyPolicyQueryOptions)).hash

  return scope.run(() => {
    const { data: tosHash } = vueQuery.useQuery(
      { ...latestTermsOfServiceQueryOptions, initialData: { hash: initialTosHash } },
      queryClient,
    )
    const { data: privacyPolicyHash } = vueQuery.useQuery(
      { ...latestPrivacyPolicyQueryOptions, initialData: { hash: initialPrivacyPolicyHash } },
      queryClient,
    )

    const agreedToTos = computed(() => tosHash.value === cachedTosHash.value?.versionHash)
    const agreedToPrivacyPolicy = computed(
      () => privacyPolicyHash.value === cachedPrivacyPolicyHash.value?.versionHash,
    )

    const onSubmit = () => {
      localStorage.set('termsOfService', { versionHash: tosHash.value })
      localStorage.set('privacyPolicy', { versionHash: privacyPolicyHash.value })
    }

    onScopeDispose(() => console.log('# scope disposed'))

    return proxyRefs({
      agreedToTos,
      agreedToPrivacyPolicy,
      onSubmit,
    })
  })
}

/** Properties of {@link AgreementsModal} component. */
export interface AgreementsModalProps {
  readonly agreedToTos: boolean
  readonly agreedToPrivacyPolicy: boolean
  readonly onSubmit: () => void
}

/** Modal for accepting the terms of service. */
export const AgreementsModal = memo(function AgreementsModal(props: AgreementsModalProps) {
  const { agreedToTos, agreedToPrivacyPolicy, onSubmit } = props
  const { getText } = useText()

  // Note that this produces warnings about missing a `<Heading slot="title">`, even though
  // all `ariaComponents.Dialog`s contain one. This is likely caused by Suspense discarding
  // renders, and so it does not seem to be fixable.
  return (
    <Dialog
      title={getText('licenseAgreementTitle')}
      isKeyboardDismissDisabled
      isDismissable={false}
      hideCloseButton
      modalProps={{ defaultOpen: true }}
      testId="agreements-modal"
      id="agreements-modal"
    >
      <Form
        schema={(schema) =>
          schema.object({
            // The user must agree to the ToS to proceed.
            agreedToTos: schema
              .array(schema.string())
              .min(1, { message: getText('licenseAgreementCheckboxError') }),
            agreedToPrivacyPolicy: schema
              .array(schema.string())
              .min(1, { message: getText('privacyPolicyCheckboxError') }),
          })
        }
        defaultValues={{
          agreedToTos: agreedToTos ? ['agree'] : [],
          agreedToPrivacyPolicy: agreedToPrivacyPolicy ? ['agree'] : [],
        }}
        testId="agreements-form"
        method="dialog"
        // onSubmit={() => {
        //   localStorage.set('termsOfService', { versionHash: tosHash })
        //   localStorage.set('privacyPolicy', { versionHash: privacyPolicyHash })
        // }}
        onSubmit={onSubmit}
      >
        {({ form }) => (
          <>
            <Text>{getText('someAgreementsHaveBeenUpdated')}</Text>

            <Checkbox.Group
              form={form}
              name="agreedToTos"
              description={
                <Button variant="link" target="_blank" href="https://ensoanalytics.com/eula">
                  {getText('viewLicenseAgreement')}
                </Button>
              }
            >
              <Checkbox value="agree">{getText('licenseAgreementCheckbox')}</Checkbox>
            </Checkbox.Group>

            <Checkbox.Group
              form={form}
              name="agreedToPrivacyPolicy"
              description={
                <Button variant="link" target="_blank" href="https://ensoanalytics.com/privacy">
                  {getText('viewPrivacyPolicy')}
                </Button>
              }
            >
              <Checkbox value="agree">{getText('privacyPolicyCheckbox')}</Checkbox>
            </Checkbox.Group>

            <Form.Submit fullWidth>{getText('accept')}</Form.Submit>

            <Form.FormError />
          </>
        )}
      </Form>
    </Dialog>
  )
})
