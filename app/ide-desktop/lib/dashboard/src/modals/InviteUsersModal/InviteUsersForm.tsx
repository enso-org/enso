/** @file A modal with inputs for user email and permission level. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'
import isEmail from 'validator/es/lib/isEmail'

import * as eventCallbackHooks from '#/hooks/eventCallbackHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'

import type * as backendModule from '#/services/Backend'

import * as parserUserEmails from '#/utilities/parseUserEmails'

// =================================
// === InviteUsersMutationParams ===
// =================================

/** Parameters for the invite users mutation. */
interface InviteUsersMutationParams {
  readonly email: backendModule.EmailAddress
  readonly organizationId: backendModule.OrganizationId
}

// =======================
// === InviteUsersForm ===
// =======================

/** Props for an {@link InviteUsersForm}. */
export interface InviteUsersFormProps {
  readonly onSubmitted: (emails: backendModule.EmailAddress[]) => void
  readonly organizationId: backendModule.OrganizationId
}

/** A modal with inputs for user email and permission level. */
export function InviteUsersForm(props: InviteUsersFormProps) {
  const { onSubmitted, organizationId } = props
  const { getText } = textProvider.useText()

  const [inputValue, setInputValue] = React.useState('')

  const backend = backendProvider.useRemoteBackendStrict()
  const inputRef = React.useRef<HTMLDivElement>(null)
  const formRef = React.useRef<HTMLFormElement>(null)

  const queryClient = reactQuery.useQueryClient()

  const inviteUserMutation = reactQuery.useMutation({
    mutationKey: ['inviteUser'],
    mutationFn: async (params: InviteUsersMutationParams) =>
      backend.inviteUser({ organizationId: params.organizationId, userEmail: params.email }),
    onSuccess: async () => {
      await queryClient.invalidateQueries({ queryKey: ['listInvitations'] })
    },
  })

  const getEmailsFromInput = eventCallbackHooks.useEventCallback((value: string) => {
    return parserUserEmails.parseUserEmails(value)
  })

  const highlightEmails = eventCallbackHooks.useEventCallback((value: string): void => {
    if (inputRef.current?.firstChild != null) {
      const trimValue = value.trim()
      const { entries } = getEmailsFromInput(value)

      // We wrap the code in a try-catch block to prevent the app from crashing
      // if the browser does not support the CSS.highlights API.
      // Currently, only Firefox doesn't support it.
      try {
        CSS.highlights.delete('field-wrong-email')

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

        CSS.highlights.set('field-wrong-email', new Highlight(...wrongEmailsRanges))
      } catch (error) {
        // ignore error
      }
    }
  })

  const validateEmailField = eventCallbackHooks.useEventCallback((value: string): string | null => {
    const trimmedValue = value.trim()
    const { entries } = getEmailsFromInput(value)

    if (trimmedValue === '' || entries.length === 0) {
      return getText('emailIsRequired')
    } else {
      for (const entry of entries) {
        if (!isEmail(entry.email)) {
          // eslint-disable-next-line no-restricted-syntax
          return getText('emailIsInvalid')
        }
      }

      return null
    }
  })

  const clearForm = eventCallbackHooks.useEventCallback(() => {
    setInputValue('')
  })

  const focusInput = eventCallbackHooks.useEventCallback(() => {
    if (inputRef.current) {
      inputRef.current.focus()
    }
  })

  React.useLayoutEffect(() => {
    highlightEmails(inputValue)
  }, [inputValue, highlightEmails])

  const emailsFieldError = validateEmailField(inputValue)
  const isEmailsFieldInvalid = emailsFieldError != null

  return (
    <aria.Form
      className="flex grow flex-col"
      ref={formRef}
      onSubmit={event => {
        event.preventDefault()

        if (isEmailsFieldInvalid) {
          highlightEmails(inputValue)
          focusInput()
        } else {
          // Add the email from the input field to the list of emails.
          const emails = Array.from(new Set(getEmailsFromInput(inputValue).entries))
            .map(({ email }) => email)
            .filter((value): value is backendModule.EmailAddress => isEmail(value))

          void Promise.all(
            emails.map(email => inviteUserMutation.mutateAsync({ email, organizationId }))
          ).then(() => {
            onSubmitted(emails)
            clearForm()
          })
        }
      }}
    >
      <aria.Text className="mb-2 text-sm text-primary">
        {getText('inviteFormDescription')}
      </aria.Text>

      <ariaComponents.ResizableContentEditableInput
        ref={inputRef}
        className="mb-4"
        name="email"
        aria-label={getText('inviteEmailFieldLabel')}
        placeholder={getText('inviteEmailFieldPlaceholder')}
        isInvalid={isEmailsFieldInvalid}
        autoComplete="off"
        value={inputValue}
        onChange={setInputValue}
        onBlur={() => {
          highlightEmails(inputValue)
          validateEmailField(inputValue)
        }}
        isRequired
        description={getText('inviteEmailFieldDescription')}
        errorMessage={emailsFieldError}
      />

      {inviteUserMutation.isError && (
        <ariaComponents.Alert variant="error" className="mb-4">
          {/* eslint-disable-next-line no-restricted-syntax */}
          {getText('arbitraryErrorTitle')}. {getText('arbitraryErrorSubtitle')}
        </ariaComponents.Alert>
      )}

      <ariaComponents.Button
        type="submit"
        variant="tertiary"
        rounded="medium"
        size="medium"
        loading={inviteUserMutation.isPending}
        fullWidth
      >
        {getText('inviteSubmit')}
      </ariaComponents.Button>
    </aria.Form>
  )
}
