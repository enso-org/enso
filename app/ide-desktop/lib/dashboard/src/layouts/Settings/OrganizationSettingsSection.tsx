/** @file Settings tab for viewing and editing account information. */
import * as React from 'react'

import isEmail from 'validator/lib/isEmail'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import SettingsInput from '#/components/styled/settings/SettingsInput'
import SettingsSection from '#/components/styled/settings/SettingsSection'

import * as backendModule from '#/services/Backend'

import * as object from '#/utilities/object'

// ===================================
// === OrganizationSettingsSection ===
// ===================================

/** Props for a {@link OrganizationSettingsSection}. */
export interface OrganizationSettingsSectionProps {
  readonly organization: backendModule.SmartOrganization | null
  readonly setOrganizationInfo: React.Dispatch<React.SetStateAction<backendModule.OrganizationInfo>>
}

/** Settings tab for viewing and editing organization information. */
export default function OrganizationSettingsSection(props: OrganizationSettingsSectionProps) {
  const { organization, setOrganizationInfo } = props
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { user } = authProvider.useNonPartialUserSession()
  const { getText } = textProvider.useText()
  const nameRef = React.useRef<HTMLInputElement | null>(null)
  const emailRef = React.useRef<HTMLInputElement | null>(null)
  const websiteRef = React.useRef<HTMLInputElement | null>(null)
  const locationRef = React.useRef<HTMLInputElement | null>(null)

  const doUpdateName = async () => {
    const oldName = organization?.value?.name ?? null
    const name = nameRef.current?.value ?? ''
    if (oldName !== name && user != null) {
      try {
        // eslint-disable-next-line @typescript-eslint/naming-convention
        setOrganizationInfo(object.merger({ name: name }))
        const newOrganization = await organization?.update({ name })
        if (newOrganization != null) {
          setOrganizationInfo(newOrganization)
        }
      } catch (error) {
        // eslint-disable-next-line @typescript-eslint/naming-convention
        setOrganizationInfo(object.merger({ name: oldName }))
        toastAndLog(null, error)
        const ref = nameRef.current
        if (ref) {
          ref.value = oldName ?? ''
        }
      }
    }
  }

  const doUpdateEmail = async () => {
    const oldEmail = organization?.value?.email ?? null
    const email = backendModule.EmailAddress(emailRef.current?.value ?? '')
    if (oldEmail !== email) {
      try {
        setOrganizationInfo(object.merger({ email }))
        const newOrganization = await organization?.update({ email })
        if (newOrganization != null) {
          setOrganizationInfo(newOrganization)
        }
      } catch (error) {
        setOrganizationInfo(object.merger({ email: oldEmail }))
        toastAndLog(null, error)
        const ref = emailRef.current
        if (ref) {
          ref.value = oldEmail ?? ''
        }
      }
    }
  }

  const doUpdateWebsite = async () => {
    const oldWebsite = organization?.value?.website ?? null
    const website = backendModule.HttpsUrl(websiteRef.current?.value ?? '')
    if (oldWebsite !== website) {
      try {
        setOrganizationInfo(object.merger({ website }))
        await organization?.update({ website })
      } catch (error) {
        setOrganizationInfo(object.merger({ website: oldWebsite }))
        toastAndLog(null, error)
        const ref = websiteRef.current
        if (ref) {
          ref.value = oldWebsite ?? ''
        }
      }
    }
  }

  const doUpdateLocation = async () => {
    const oldLocation = organization?.value?.address ?? null
    const location = locationRef.current?.value ?? ''
    if (oldLocation !== location) {
      try {
        setOrganizationInfo(object.merger({ address: location }))
        const newOrganization = await organization?.update({ address: location })
        if (newOrganization != null) {
          setOrganizationInfo(newOrganization)
        }
      } catch (error) {
        setOrganizationInfo(object.merger({ address: oldLocation }))
        toastAndLog(null, error)
        const ref = locationRef.current
        if (ref) {
          ref.value = oldLocation ?? ''
        }
      }
    }
  }

  return (
    <SettingsSection title={getText('organization')}>
      <div key={JSON.stringify(organization)} className="flex flex-col">
        <aria.TextField
          key={organization?.value?.name ?? 'organization name'}
          defaultValue={organization?.value?.name ?? ''}
          className="flex h-row gap-settings-entry"
        >
          <aria.Label className="text my-auto w-organization-settings-label">
            {getText('organizationDisplayName')}
          </aria.Label>
          <SettingsInput
            key={organization?.value?.name}
            ref={nameRef}
            type="text"
            onSubmit={doUpdateName}
          />
        </aria.TextField>
        <aria.TextField
          key={organization?.value?.email ?? 'organization email'}
          defaultValue={organization?.value?.email ?? ''}
          className="flex h-row gap-settings-entry"
        >
          <aria.Label className="text my-auto w-organization-settings-label">
            {getText('email')}
          </aria.Label>
          <SettingsInput
            key={organization?.value?.email}
            ref={emailRef}
            type="text"
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
        <aria.TextField
          key={organization?.value?.website ?? 'organization website'}
          defaultValue={organization?.value?.website ?? ''}
          className="flex h-row gap-settings-entry"
        >
          <aria.Label className="text my-auto w-organization-settings-label">
            {getText('website')}
          </aria.Label>
          <SettingsInput
            key={organization?.value?.website}
            ref={websiteRef}
            type="text"
            onSubmit={doUpdateWebsite}
          />
        </aria.TextField>
        <aria.TextField
          key={organization?.value?.address ?? 'organization address'}
          defaultValue={organization?.value?.address ?? ''}
          className="flex h-row gap-settings-entry"
        >
          <aria.Label className="text my-auto w-organization-settings-label">
            {getText('location')}
          </aria.Label>
          <SettingsInput
            ref={locationRef}
            key={organization?.value?.address}
            type="text"
            onSubmit={doUpdateLocation}
          />
        </aria.TextField>
      </div>
    </SettingsSection>
  )
}
