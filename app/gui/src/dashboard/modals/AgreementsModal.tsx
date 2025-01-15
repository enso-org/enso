/** @file Modal for accepting the terms of service and privacy policy. */
import { queryOptions, useSuspenseQuery } from '@tanstack/react-query'
import { Outlet } from 'react-router'
import * as z from 'zod'

import {
  useAcceptedPrivacyPolicyVersionState,
  useAcceptedTermsOfServiceVersionState,
} from '#/appLocalStorage'
import { Button, Checkbox, Dialog, Form, Text } from '#/components/AriaComponents'
import { useAuth } from '#/providers/AuthProvider'
import { useText } from '#/providers/TextProvider'

const TEN_MINUTES_MS = 600_000
const TOS_ENDPOINT_SCHEMA = z.object({ hash: z.string() })
const PRIVACY_POLICY_ENDPOINT_SCHEMA = z.object({ hash: z.string() })

export const latestTermsOfServiceQueryOptions = queryOptions({
  queryKey: ['termsOfService', 'currentVersion'],
  queryFn: async () => {
    const response = await fetch(new URL('/eula.json', $config.ENSO_HOST))
    if (!response.ok) {
      throw new Error('Failed to fetch Terms of Service')
    } else {
      return TOS_ENDPOINT_SCHEMA.parse(await response.json())
    }
  },
  refetchOnWindowFocus: true,
  refetchIntervalInBackground: true,
  refetchInterval: TEN_MINUTES_MS,
})

export const latestPrivacyPolicyQueryOptions = queryOptions({
  queryKey: ['privacyPolicy', 'currentVersion'],
  queryFn: async () => {
    const response = await fetch(new URL('/privacy.json', $config.ENSO_HOST))
    if (!response.ok) {
      throw new Error('Failed to fetch Privacy Policy')
    } else {
      return PRIVACY_POLICY_ENDPOINT_SCHEMA.parse(await response.json())
    }
  },
  refetchOnWindowFocus: true,
  refetchIntervalInBackground: true,
  refetchInterval: TEN_MINUTES_MS,
})

/** Modal for accepting the terms of service. */
export function AgreementsModal() {
  const { getText } = useText()
  const { session } = useAuth()

  const [cachedTosHash, setCachedTosHash] = useAcceptedTermsOfServiceVersionState()
  const [cachedPrivacyPolicyHash, setCachedPrivacyPolicyHash] =
    useAcceptedPrivacyPolicyVersionState()

  const { data: tosHash } = useSuspenseQuery({
    ...latestTermsOfServiceQueryOptions,
    // If the user has already accepted the EULA, we don't need to
    // block user interaction with the app while we fetch the latest version.
    // We can use the local version hash as the initial data.
    // and refetch in the background to check for updates.
    ...(cachedTosHash?.versionHash != null && {
      initialData: { hash: cachedTosHash.versionHash },
    }),
    select: (data) => data.hash,
  })
  const { data: privacyPolicyHash } = useSuspenseQuery({
    ...latestPrivacyPolicyQueryOptions,
    ...(cachedPrivacyPolicyHash?.versionHash != null && {
      initialData: { hash: cachedPrivacyPolicyHash.versionHash },
    }),
    select: (data) => data.hash,
  })

  const isLatest =
    tosHash === cachedTosHash?.versionHash &&
    privacyPolicyHash === cachedPrivacyPolicyHash?.versionHash

  const isAccepted = cachedTosHash != null
  const shouldDisplay = !(isAccepted && isLatest)

  if (shouldDisplay) {
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
            agreedToTos: tosHash === cachedTosHash?.versionHash ? ['agree'] : [],
            agreedToPrivacyPolicy:
              privacyPolicyHash === cachedPrivacyPolicyHash?.versionHash ? ['agree'] : [],
          }}
          testId="agreements-form"
          method="dialog"
          onSubmit={() => {
            setCachedTosHash({ versionHash: tosHash })
            setCachedPrivacyPolicyHash({ versionHash: privacyPolicyHash })
          }}
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
  }

  return <Outlet context={session} />
}
