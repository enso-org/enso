/** @file Modal for accepting the terms of service. */

import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'
import * as router from 'react-router'
import * as z from 'zod'

import * as authProvider from '#/providers/AuthProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'

import LocalStorage from '#/utilities/LocalStorage'
import * as object from '#/utilities/object'
import * as tailwindMerge from '#/utilities/tailwindMerge'

// =================
// === Constants ===
// =================

const TEN_MINUTES_MS = 600_000
const TERMS_OF_SERVICE_SCHEMA = z.object({ versionHash: z.string() })
const TOS_ENDPOINT_SCHEMA = z.object({ hash: z.string() })

// ============================
// === Global configuration ===
// ============================

declare module '#/utilities/LocalStorage' {
  /** Metadata containing the version hash of the terms of service that the user has accepted. */
  interface LocalStorageData {
    readonly termsOfService: z.infer<typeof TERMS_OF_SERVICE_SCHEMA>
  }
}

LocalStorage.registerKey('termsOfService', { schema: TERMS_OF_SERVICE_SCHEMA })

export const latestTermsOfServiceQueryOptions = reactQuery.queryOptions({
  queryKey: ['termsOfService', 'currentVersion'],
  queryFn: async () => {
    const response = await fetch(new URL('/eula.json', process.env.ENSO_CLOUD_ENSO_HOST))
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

// ===========================
// === TermsOfServiceModal ===
// ===========================

/** Modal for accepting the terms of service. */
export function TermsOfServiceModal() {
  const { getText } = textProvider.useText()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const checkboxId = React.useId()
  const { session } = authProvider.useAuth()

  const localVersionHash = localStorage.get('termsOfService')?.versionHash
  const { data: latestVersionHash } = reactQuery.useSuspenseQuery({
    ...latestTermsOfServiceQueryOptions,
    // If the user has already accepted the EULA, we don't need to
    // block user interaction with the app while we fetch the latest version.
    // We can use the local version hash as the initial data.
    // and refetch in the background to check for updates.
    ...(localVersionHash != null && {
      initialData: { hash: localVersionHash },
    }),
    select: (data) => data.hash,
  })

  const isLatest = latestVersionHash === localVersionHash
  const isAccepted = localVersionHash != null
  const shouldDisplay = !(isAccepted && isLatest)

  const formSchema = ariaComponents.Form.useFormSchema((schema) =>
    schema.object({
      agreed: schema
        .boolean()
        // The user must agree to the ToS to proceed.
        .refine((value) => value, getText('licenseAgreementCheckboxError')),
      hash: schema.string(),
    }),
  )

  if (shouldDisplay) {
    // Note that this produces warnings about missing a `<Heading slot="title">`, even though
    // all `ariaComponents.Dialog`s contain one. This is likely caused by Suspense discarding
    // renders, and so it does not seem to be fixable.
    return (
      <ariaComponents.Dialog
        title={getText('licenseAgreementTitle')}
        isKeyboardDismissDisabled
        isDismissable={false}
        hideCloseButton
        modalProps={{ defaultOpen: true }}
        testId="terms-of-service-modal"
        id="terms-of-service-modal"
      >
        <ariaComponents.Form
          schema={formSchema}
          defaultValues={{ agreed: false, hash: latestVersionHash }}
          testId="terms-of-service-form"
          method="dialog"
          onSubmit={({ hash }) => {
            localStorage.set('termsOfService', { versionHash: hash }, { triggerRerender: true })
          }}
        >
          {({ register }) => (
            <>
              <ariaComponents.Form.Field name="agreed">
                {({ isInvalid }) => (
                  <>
                    <div className="flex w-full items-center gap-1">
                      <aria.Input
                        type="checkbox"
                        className={tailwindMerge.twMerge(
                          'flex size-4 cursor-pointer overflow-clip rounded-lg border border-primary outline-primary focus-visible:outline focus-visible:outline-2',
                          isInvalid && 'border-red-700 text-red-500 outline-red-500',
                        )}
                        id={checkboxId}
                        data-testid="terms-of-service-checkbox"
                        {...object.omit(register('agreed'), 'isInvalid')}
                      />

                      <label htmlFor={checkboxId}>
                        <ariaComponents.Text>
                          {getText('licenseAgreementCheckbox')}
                        </ariaComponents.Text>
                      </label>
                    </div>

                    <ariaComponents.Button
                      variant="link"
                      target="_blank"
                      href="https://ensoanalytics.com/eula"
                    >
                      {getText('viewLicenseAgreement')}
                    </ariaComponents.Button>
                  </>
                )}
              </ariaComponents.Form.Field>

              <ariaComponents.Form.FormError />

              <ariaComponents.Form.Submit fullWidth>{getText('accept')}</ariaComponents.Form.Submit>
            </>
          )}
        </ariaComponents.Form>
      </ariaComponents.Dialog>
    )
  } else {
    return <router.Outlet context={session} />
  }
}
