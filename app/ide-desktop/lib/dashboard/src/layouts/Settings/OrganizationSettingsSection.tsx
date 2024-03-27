/** @file Settings tab for viewing and editing account information. */
import * as React from 'react'

import isEmail from 'validator/lib/isEmail'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import SettingsInput from '#/layouts/Settings/SettingsInput'

import * as aria from '#/components/aria'
import FocusArea from '#/components/styled/FocusArea'

import * as backendModule from '#/services/Backend'

import * as object from '#/utilities/object'

// ===================================
// === OrganizationSettingsSection ===
// ===================================

/** Props for a {@link OrganizationSettingsSection}. */
export interface OrganizationSettingsSectionProps {
  readonly organization: backendModule.OrganizationInfo
  readonly setOrganization: React.Dispatch<React.SetStateAction<backendModule.OrganizationInfo>>
}

/** Settings tab for viewing and editing organization information. */
export default function OrganizationSettingsSection(props: OrganizationSettingsSectionProps) {
  const { organization, setOrganization } = props
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { backend } = backendProvider.useBackend()
  const { getText } = textProvider.useText()
  const nameRef = React.useRef<HTMLInputElement | null>(null)
  const emailRef = React.useRef<HTMLInputElement | null>(null)
  const websiteRef = React.useRef<HTMLInputElement | null>(null)
  const locationRef = React.useRef<HTMLInputElement | null>(null)

  const doUpdateName = async () => {
    const oldName = organization.organization_name ?? null
    const name = nameRef.current?.value ?? ''
    if (oldName !== name) {
      try {
        // eslint-disable-next-line @typescript-eslint/naming-convention
        setOrganization(object.merger({ organization_name: name }))
        const newOrganization = await backend.updateOrganization({ name })
        if (newOrganization != null) {
          setOrganization(newOrganization)
        }
      } catch (error) {
        // eslint-disable-next-line @typescript-eslint/naming-convention
        setOrganization(object.merger({ organization_name: oldName }))
        toastAndLog(null, error)
        const ref = nameRef.current
        if (ref) {
          ref.value = oldName ?? ''
        }
      }
    }
  }

  const doUpdateEmail = async () => {
    const oldEmail = organization.email ?? null
    const email = backendModule.EmailAddress(emailRef.current?.value ?? '')
    if (oldEmail !== email) {
      try {
        setOrganization(object.merger({ email }))
        const newOrganization = await backend.updateOrganization({ email })
        if (newOrganization != null) {
          setOrganization(newOrganization)
        }
      } catch (error) {
        setOrganization(object.merger({ email: oldEmail }))
        toastAndLog(null, error)
        const ref = emailRef.current
        if (ref) {
          ref.value = oldEmail ?? ''
        }
      }
    }
  }

  const doUpdateWebsite = async () => {
    const oldWebsite = organization.website ?? null
    const website = backendModule.HttpsUrl(websiteRef.current?.value ?? '')
    if (oldWebsite !== website) {
      try {
        setOrganization(object.merger({ website }))
        await backend.updateOrganization({ website })
      } catch (error) {
        setOrganization(object.merger({ website: oldWebsite }))
        toastAndLog(null, error)
        const ref = websiteRef.current
        if (ref) {
          ref.value = oldWebsite ?? ''
        }
      }
    }
  }

  const doUpdateLocation = async () => {
    const oldLocation = organization.address ?? null
    const location = locationRef.current?.value ?? ''
    if (oldLocation !== location) {
      try {
        setOrganization(object.merger({ address: location }))
        const newOrganization = await backend.updateOrganization({ location })
        if (newOrganization != null) {
          setOrganization(newOrganization)
        }
      } catch (error) {
        setOrganization(object.merger({ address: oldLocation }))
        toastAndLog(null, error)
        const ref = locationRef.current
        if (ref) {
          ref.value = oldLocation ?? ''
        }
      }
    }
  }

  return (
    <FocusArea direction="vertical">
      {(ref, innerProps) => (
        <div ref={ref} className="flex flex-col gap-settings-section-header" {...innerProps}>
          <aria.Heading className="settings-subheading">{getText('organization')}</aria.Heading>
          <div className="flex flex-col">
            <aria.TextField className="flex h-row gap-settings-entry">
              <aria.Label className="text my-auto w-organization-settings-label">
                {getText('organizationDisplayName')}
              </aria.Label>
              <SettingsInput
                key={organization.organization_name}
                ref={nameRef}
                type="text"
                initialValue={organization.organization_name ?? ''}
                onSubmit={doUpdateName}
              />
            </aria.TextField>
            <aria.TextField className="flex h-row gap-settings-entry">
              <aria.Label className="text my-auto w-organization-settings-label">
                {getText('email')}
              </aria.Label>
              <SettingsInput
                key={organization.email}
                ref={emailRef}
                type="text"
                initialValue={organization.email ?? ''}
                onSubmit={value => {
                  if (isEmail(value)) {
                    void doUpdateEmail()
                  } else {
                    emailRef.current?.focus()
                  }
                }}
                onChange={() => {
                  emailRef.current?.setCustomValidity(
                    isEmail(emailRef.current.value) ? '' : 'Invalid email.'
                  )
                }}
              />
            </aria.TextField>
            <aria.TextField className="flex h-row gap-settings-entry">
              <aria.Label className="text my-auto w-organization-settings-label">
                {getText('website')}
              </aria.Label>
              <SettingsInput
                key={organization.website}
                ref={websiteRef}
                type="text"
                initialValue={organization.website ?? ''}
                onSubmit={doUpdateWebsite}
              />
            </aria.TextField>
            <aria.TextField className="flex h-row gap-settings-entry">
              <aria.Label className="text my-auto w-organization-settings-label">
                {getText('location')}
              </aria.Label>
              <SettingsInput
                ref={locationRef}
                key={organization.address}
                type="text"
                initialValue={organization.address ?? ''}
                onSubmit={doUpdateLocation}
              />
            </aria.TextField>
          </div>
        </div>
      )}
    </FocusArea>
  )
}
