/** @file A modal with inputs for user email and permission level. */
import * as React from 'react'

import { useMutation, useSuspenseQuery } from '@tanstack/react-query'
import isEmail from 'validator/es/lib/isEmail'

import { backendMutationOptions } from '#/hooks/backendHooks'
import * as billingHooks from '#/hooks/billing'
import * as eventCallbackHooks from '#/hooks/eventCallbackHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import * as paywallComponents from '#/components/Paywall'

import type * as backendModule from '#/services/Backend'

import * as parserUserEmails from '#/utilities/parseUserEmails'

// =======================
// === InviteUsersForm ===
// =======================

/** Props for an {@link InviteUsersForm}. */
export interface InviteUsersFormProps {
  readonly onSubmitted: (emails: backendModule.EmailAddress[]) => void
}

/** A modal with inputs for user email and permission level. */
export function InviteUsersForm(props: InviteUsersFormProps) {
  const { onSubmitted } = props
  const { getText } = textProvider.useText()
  const backend = backendProvider.useRemoteBackend()
  const inputRef = React.useRef<HTMLDivElement>(null)

  const { user } = authProvider.useFullUserSession()
  const { isFeatureUnderPaywall } = billingHooks.usePaywall({ plan: user.plan })

  const inviteUserMutation = useMutation(
    backendMutationOptions(backend, 'inviteUser', {
      meta: { invalidates: [['listInvitations']], awaitInvalidates: true },
    }),
  )

  const { data: invitations } = useSuspenseQuery({
    queryKey: ['listInvitations'],
    queryFn: async () => backend.listInvitations(),
    select: (data) => ({
      count: data.invitations.length,
      availableLicenses: data.availableLicenses,
    }),
  })

  const isUnderPaywall = isFeatureUnderPaywall('inviteUserFull')
  const seatsLeft = invitations.availableLicenses

  const getEmailsFromInput = eventCallbackHooks.useEventCallback((value: string) =>
    parserUserEmails.parseUserEmails(value),
  )

  const highlightEmails = eventCallbackHooks.useEventCallback((value: string): void => {
    if (inputRef.current?.firstChild != null) {
      const trimValue = value.trim()
      const { entries } = getEmailsFromInput(value)

      if (typeof Highlight !== 'undefined') {
        CSS.highlights.delete('field-wrong-email')
      }

      let offset = 0

      const wrongEmailsRanges: Range[] = []

      for (const entry of entries) {
        const emailIndex = trimValue.indexOf(entry.email, offset)

        const range = new Range()
        range.setStart(inputRef.current.firstChild, emailIndex)
        range.setEnd(inputRef.current.firstChild, emailIndex + entry.email.length)

        if (!isEmail(entry.email)) {
          wrongEmailsRanges.push(range)
        }

        offset = emailIndex + entry.email.length
      }

      if (typeof Highlight !== 'undefined') {
        CSS.highlights.set('field-wrong-email', new Highlight(...wrongEmailsRanges))
      }
    }
  })

  const validateEmailField = eventCallbackHooks.useEventCallback((value: string): string | null => {
    const { entries } = getEmailsFromInput(value)

    if (entries.length > seatsLeft) {
      return getText('inviteFormSeatsLeftError', entries.length - seatsLeft)
    } else {
      for (const entry of entries) {
        if (!isEmail(entry.email)) {
          return getText('emailIsInvalid')
        }
      }

      return null
    }
  })

  return (
    <ariaComponents.Form
      formOptions={{ mode: 'onSubmit' }}
      schema={ariaComponents.Form.schema.object({
        emails: ariaComponents.Form.schema
          .string()
          .min(1, { message: getText('emailIsRequired') })
          .refine(
            (value) => {
              const result = validateEmailField(value)

              if (result != null) {
                highlightEmails(value)
              }

              return result == null
            },
            { message: getText('emailIsInvalid') },
          ),
      })}
      defaultValues={{ emails: '' }}
      onSubmit={async ({ emails }) => {
        // Add the email from the input field to the list of emails.
        const emailsToSubmit = Array.from(new Set(getEmailsFromInput(emails).entries))
          .map(({ email }) => email)
          .filter((value): value is backendModule.EmailAddress => isEmail(value))

        await Promise.all(
          emailsToSubmit.map((userEmail) => inviteUserMutation.mutateAsync([{ userEmail }])),
        ).then(() => {
          onSubmitted(emailsToSubmit)
        })
      }}
    >
      <ariaComponents.ResizableContentEditableInput
        ref={inputRef}
        name="emails"
        label={getText('inviteEmailFieldLabel')}
        placeholder={getText('inviteEmailFieldPlaceholder')}
        description={getText('inviteEmailFieldDescription')}
      />

      {isUnderPaywall && (
        <paywallComponents.PaywallAlert
          feature="inviteUserFull"
          label={getText('inviteFormSeatsLeft', seatsLeft)}
        />
      )}

      <ariaComponents.Form.Submit
        variant="accent"
        size="medium"
        fullWidth
        isDisabled={seatsLeft <= 0}
      >
        {getText('inviteSubmit')}
      </ariaComponents.Form.Submit>

      <ariaComponents.Form.FormError />
    </ariaComponents.Form>
  )
}
