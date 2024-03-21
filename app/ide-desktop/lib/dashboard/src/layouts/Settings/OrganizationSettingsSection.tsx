/** @file Settings tab for viewing and editing account information. */
import * as React from 'react'

import isEmail from 'validator/lib/isEmail'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as backendProvider from '#/providers/BackendProvider'

import SettingsInput from '#/layouts/Settings/SettingsInput'

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
          <h3 className="settings-subheading">Organization</h3>
          <div className="flex flex-col">
            <div className="flex h-row gap-settings-entry">
              <span className="text my-auto w-organization-settings-label">
                Organization display name
              </span>
              <span className="text my-auto grow font-bold">
                <SettingsInput
                  key={organization.organization_name}
                  type="text"
                  initialValue={organization.organization_name ?? ''}
                  onSubmit={doUpdateName}
                />
              </span>
            </div>
            <div className="flex h-row gap-settings-entry">
              <span className="text my-auto w-organization-settings-label">Email</span>
              <span className="text my-auto grow font-bold">
                <SettingsInput
                  key={organization.email}
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
              </span>
            </div>
            <div className="flex h-row gap-settings-entry">
              <span className="text my-auto w-organization-settings-label">Website</span>
              <span className="text my-auto grow font-bold">
                <SettingsInput
                  key={organization.website}
                  type="text"
                  initialValue={organization.website ?? ''}
                  onSubmit={doUpdateWebsite}
                />
              </span>
            </div>
            <div className="flex h-row gap-settings-entry">
              <span className="text my-auto w-organization-settings-label">Location</span>
              <span className="text my-auto grow font-bold">
                <SettingsInput
                  key={organization.address}
                  type="text"
                  initialValue={organization.address ?? ''}
                  onSubmit={doUpdateLocation}
                />
              </span>
            </div>
          </div>
        </div>
      )}
    </FocusArea>
  )
}
