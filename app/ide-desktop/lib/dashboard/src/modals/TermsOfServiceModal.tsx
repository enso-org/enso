/**
 * @file
 *
 * Modal for accepting the terms of service.
 */

import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'
import * as router from 'react-router'
import * as twMerge from 'tailwind-merge'
import * as z from 'zod'

import * as authProvider from '#/providers/AuthProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'

import LocalStorage from '#/utilities/LocalStorage'

declare module '#/utilities/LocalStorage' {
  /**
   * Contains the latest terms of service version hash that the user has accepted.
   */
  interface LocalStorageData {
    readonly termsOfService: z.infer<typeof TERMS_OF_SERVICE_SCHEMA> | null
  }
}

const TERMS_OF_SERVICE_SCHEMA = z.object({ versionHash: z.string() })
LocalStorage.registerKey('termsOfService', { schema: TERMS_OF_SERVICE_SCHEMA })

export const latestTermsOfService = reactQuery.queryOptions({
  queryKey: ['termsOfService', 'currentVersion'],
  queryFn: () =>
    fetch(new URL('/eula.json', process.env.ENSO_CLOUD_ENSO_HOST))
      .then(response => {
        if (!response.ok) {
          throw new Error('Failed to fetch terms of service')
        } else {
          return response.json()
        }
      })
      .then(data => {
        const schema = z.object({ hash: z.string() })
        return schema.parse(data)
      }),
  refetchOnWindowFocus: true,
  refetchIntervalInBackground: true,
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  refetchInterval: 1000 * 60 * 10, // 10 minutes
})

/**
 * Modal for accepting the terms of service.
 */
export function TermsOfServiceModal() {
  const { getText } = textProvider.useText()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const checkboxId = React.useId()
  const { session } = authProvider.useAuth()

  const localVersionHash = localStorage.get('termsOfService')?.versionHash
  const { data: latestVersionHash } = reactQuery.useSuspenseQuery({
    ...latestTermsOfService,
    // If the user has already accepted EULA, we don't need to
    // block user interaction with the app while we fetch the latest version.
    // We can use the local version hash as the initial data.
    // and refetch in the background to check for updates.
    ...(localVersionHash != null && {
      initialData: { hash: localVersionHash },
      initialDataUpdatedAt: 0,
    }),
    select: data => data.hash,
  })

  const isLatest = latestVersionHash === localVersionHash
  const isAccepted = localVersionHash != null
  const shouldDisplay = !(isAccepted && isLatest)

  const formSchema = ariaComponents.Form.useFormSchema(schema =>
    schema.object({
      agree: schema
        .boolean()
        // we accept only true
        .refine(value => value, getText('licenseAgreementCheckboxError')),
      hash: schema.string(),
    })
  )

  if (shouldDisplay) {
    return (
      <>
        <ariaComponents.Dialog
          title={getText('licenseAgreementTitle')}
          isKeyboardDismissDisabled
          isDismissable={false}
          hideCloseButton
          modalProps={{ isOpen: true }}
          testId="terms-of-service-modal"
          id="terms-of-service-modal"
        >
          <ariaComponents.Form
            schema={formSchema}
            defaultValues={{ agree: false, hash: latestVersionHash }}
            testId="terms-of-service-form"
            onSubmit={({ hash }) => {
              localStorage.set('termsOfService', { versionHash: hash })
            }}
          >
            {({ register, formState }) => {
              const agreeError = formState.errors.agree
              const hasError = agreeError != null

              return (
                <>
                  <div>
                    <div className="mb-1">
                      <div className="flex items-center gap-1.5 text-sm">
                        <aria.Input
                          type="checkbox"
                          className={twMerge.twMerge(
                            `flex size-4 cursor-pointer overflow-clip rounded-lg border border-primary outline-primary focus-visible:outline focus-visible:outline-2 ${hasError ? 'border-red-700 text-red-500 outline-red-500' : ''}`
                          )}
                          id={checkboxId}
                          aria-invalid={hasError}
                          data-testid="terms-of-service-checkbox"
                          {...register('agree')}
                        />

                        <aria.Label htmlFor={checkboxId} className="text-sm">
                          {getText('licenseAgreementCheckbox')}
                        </aria.Label>
                      </div>

                      {agreeError && (
                        <p className="m-0 text-xs text-red-700" role="alert">
                          {agreeError.message}
                        </p>
                      )}
                    </div>

                    <ariaComponents.Button
                      variant="link"
                      target="_blank"
                      href="https://enso.org/eula"
                    >
                      {getText('viewLicenseAgreement')}
                    </ariaComponents.Button>
                  </div>

                  <ariaComponents.Form.FormError />

                  <ariaComponents.Form.Submit>{getText('accept')}</ariaComponents.Form.Submit>
                </>
              )
            }}
          </ariaComponents.Form>
        </ariaComponents.Dialog>
      </>
    )
  } else {
    return <router.Outlet context={session} />
  }
}
