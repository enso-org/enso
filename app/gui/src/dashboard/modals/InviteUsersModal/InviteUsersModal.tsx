/** @file A modal with inputs for user email and permission level. */
import * as React from 'react'

import * as authProvider from '#/providers/AuthProvider'
import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'

import * as inviteUsersForm from '#/modals/InviteUsersModal/InviteUsersForm'
import * as inviteUsersSuccess from '#/modals/InviteUsersModal/InviteUsersSuccess'

import { Stepper } from '#/components/Stepper'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import type * as backendModule from '#/services/Backend'

// ========================
// === InviteUsersModal ===
// ========================

/** Props for an {@link InviteUsersModal}. */
export interface InviteUsersModalProps {
  readonly relativeToTrigger?: boolean
}

/** A modal for inviting one or more users. */
export default function InviteUsersModal(props: InviteUsersModalProps) {
  const { relativeToTrigger = false } = props
  const { getText } = textProvider.useText()
  const { user } = authProvider.useFullUserSession()

  if (relativeToTrigger) {
    return (
      <ariaComponents.Popover>
        <InviteUsersModalContent organizationId={user.organizationId} />
      </ariaComponents.Popover>
    )
  } else {
    return (
      <ariaComponents.Dialog title={getText('invite')}>
        {({ close }) => (
          <InviteUsersModalContent organizationId={user.organizationId} onClose={close} />
        )}
      </ariaComponents.Dialog>
    )
  }
}

// ===============================
// === InviteUsersModalContent ===
// ===============================

/** Props for the content of an {@link InviteUsersModal}. */
interface InviteUsersModalContentProps {
  readonly onClose?: () => void
  readonly organizationId: backendModule.OrganizationId
}

/** The content of an {@link InviteUsersModal}. */
function InviteUsersModalContent(props: InviteUsersModalContentProps) {
  const { organizationId } = props

  const { stepperState, nextStep } = Stepper.useStepperState({
    steps: 2,
  })

  const [submittedEmails, setSubmittedEmails] = React.useState<string[]>([])
  const onInviteUsersFormInviteUsersFormSubmitted = useEventCallback(
    (emails: backendModule.EmailAddress[]) => {
      nextStep()
      setSubmittedEmails(emails)
    },
  )

  const invitationParams = new URLSearchParams({
    // eslint-disable-next-line @typescript-eslint/naming-convention, camelcase
    organization_id: organizationId,
  }).toString()
  const invitationLink = `enso://auth/registration?${invitationParams}`

  return (
    <Stepper state={stepperState} renderStep={() => null}>
      <Stepper.StepContent index={0}>
        <inviteUsersForm.InviteUsersForm onSubmitted={onInviteUsersFormInviteUsersFormSubmitted} />
      </Stepper.StepContent>

      <Stepper.StepContent index={1}>
        <inviteUsersSuccess.InviteUsersSuccess
          {...props}
          invitationLink={invitationLink}
          emails={submittedEmails}
        />
      </Stepper.StepContent>
    </Stepper>
  )
}
