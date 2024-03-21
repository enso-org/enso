/** @file Settings tab for viewing and editing account information. */
import * as React from 'react'

import isEmail from 'validator/lib/isEmail'

import DefaultUserIcon from 'enso-assets/default_user.svg'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as backendProvider from '#/providers/BackendProvider'

import * as backendModule from '#/services/Backend'

import * as object from '#/utilities/object'

// ===============================
// === OrganizationSettingsTab ===
// ===============================

/** Props for a {@link OrganizationSettingsTab}. */
export interface OrganizationSettingsTabProps {
  readonly organization: backendModule.OrganizationInfo
  readonly setOrganization: React.Dispatch<React.SetStateAction<backendModule.OrganizationInfo>>
}

/** Settings tab for viewing and editing organization information. */
export default function OrganizationSettingsTab(props: OrganizationSettingsTabProps) {
  const { organization, setOrganization } = props
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { backend } = backendProvider.useBackend()
  const nameRef = React.useRef<HTMLInputElement>(null)
  const emailRef = React.useRef<HTMLInputElement>(null)
  const websiteRef = React.useRef<HTMLInputElement>(null)
  const locationRef = React.useRef<HTMLInputElement>(null)

  const doUpdateName = async () => {
    const oldName = organization.name ?? null
    const name = nameRef.current?.value ?? ''
    if (oldName !== name) {
      try {
        // eslint-disable-next-line @typescript-eslint/naming-convention
        setOrganization(object.merger({ name }))
        const newOrganization = await backend.updateOrganization({ name })
        if (newOrganization != null) {
          setOrganization(newOrganization)
        }
      } catch (error) {
        // eslint-disable-next-line @typescript-eslint/naming-convention
        setOrganization(object.merger({ name: oldName }))
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

  const doUploadOrganizationPicture = async (event: React.ChangeEvent<HTMLInputElement>) => {
    const image = event.target.files?.[0]
    if (image == null) {
      toastAndLog('Could not upload a new profile picture because no image was found')
    } else {
      try {
        const newOrganization = await backend.uploadOrganizationPicture(
          { fileName: image.name },
          image
        )
        setOrganization(newOrganization)
      } catch (error) {
        toastAndLog(null, error)
      }
    }
    // Reset selected files, otherwise the file input will do nothing if the same file is
    // selected again. While technically not undesired behavior, it is unintuitive for the user.
    event.target.value = ''
  }

  const onKeyDown = (event: React.KeyboardEvent<HTMLInputElement>, defaultValue: string) => {
    switch (event.key) {
      case 'Escape': {
        event.stopPropagation()
        event.currentTarget.value = defaultValue
        event.currentTarget.blur()
        break
      }
      case 'Enter': {
        event.stopPropagation()
        event.currentTarget.blur()
        break
      }
      case 'Tab': {
        event.currentTarget.blur()
        break
      }
    }
  }

  return (
    <div className="flex-0 flex h flex-col gap-settings-section lg:h-auto lg:flex-row">
      <div className="flex w-settings-main-section flex-col gap-settings-subsection">
        <div className="flex flex-col gap-settings-section-header">
          <h3 className="settings-subheading">Organization</h3>
          <div className="flex flex-col">
            <div className="flex h-row gap-settings-entry">
              <span className="text my-auto w-organization-settings-label">
                Organization display name
              </span>
              <span className="text my-auto grow font-bold">
                <input
                  ref={nameRef}
                  className="settings-value w-full rounded-full bg-transparent font-bold placeholder-black/30 transition-colors invalid:border invalid:border-red-700 hover:bg-selected-frame focus:bg-selected-frame"
                  key={organization.name}
                  type="text"
                  size={1}
                  defaultValue={organization.name ?? ''}
                  onBlur={doUpdateName}
                  onKeyDown={event => {
                    onKeyDown(event, organization.name ?? '')
                  }}
                />
              </span>
            </div>
            <div className="flex h-row gap-settings-entry">
              <span className="text my-auto w-organization-settings-label">Email</span>
              <span className="text my-auto grow font-bold">
                <input
                  ref={emailRef}
                  className="settings-value w-full rounded-full bg-transparent font-bold placeholder-black/30 transition-colors invalid:border invalid:border-red-700 hover:bg-selected-frame focus:bg-selected-frame"
                  key={organization.email}
                  type="text"
                  size={1}
                  defaultValue={organization.email ?? ''}
                  onBlur={event => {
                    if (isEmail(event.currentTarget.value)) {
                      void doUpdateEmail()
                    } else {
                      event.currentTarget.focus()
                    }
                  }}
                  onKeyDown={event => {
                    onKeyDown(event, organization.email ?? '')
                  }}
                  onInput={event => {
                    event.currentTarget.setCustomValidity(
                      isEmail(event.currentTarget.value) ? '' : 'Invalid email.'
                    )
                  }}
                />
              </span>
            </div>
            <div className="flex h-row gap-settings-entry">
              <span className="text my-auto w-organization-settings-label">Website</span>
              <span className="text my-auto grow font-bold">
                <input
                  ref={websiteRef}
                  className="settings-value w-full rounded-full bg-transparent font-bold placeholder-black/30 transition-colors invalid:border invalid:border-red-700 hover:bg-selected-frame focus:bg-selected-frame"
                  key={organization.website}
                  type="text"
                  size={1}
                  defaultValue={organization.website ?? ''}
                  onBlur={doUpdateWebsite}
                  onKeyDown={event => {
                    onKeyDown(event, organization.website ?? '')
                  }}
                />
              </span>
            </div>
            <div className="flex h-row gap-settings-entry">
              <span className="text my-auto w-organization-settings-label">Location</span>
              <span className="text my-auto grow font-bold">
                <input
                  ref={locationRef}
                  className="settings-value w-full rounded-full bg-transparent font-bold placeholder-black/30 transition-colors invalid:border invalid:border-red-700 hover:bg-selected-frame focus:bg-selected-frame"
                  key={organization.address}
                  type="text"
                  size={1}
                  defaultValue={organization.address ?? ''}
                  onBlur={doUpdateLocation}
                  onKeyDown={event => {
                    onKeyDown(event, organization.address ?? '')
                  }}
                />
              </span>
            </div>
          </div>
        </div>
      </div>
      <div className="flex flex-col gap-settings-section-header">
        <h3 className="settings-subheading">Profile picture</h3>
        <label className="flex h-profile-picture-large w-profile-picture-large cursor-pointer items-center overflow-clip rounded-full transition-colors hover:bg-frame">
          <input
            type="file"
            className="hidden"
            accept="image/*"
            onChange={doUploadOrganizationPicture}
          />
          <img
            src={organization.picture ?? DefaultUserIcon}
            width={128}
            height={128}
            className="pointer-events-none"
          />
        </label>
        <span className="w-profile-picture-caption py-profile-picture-caption-y">
          Your organization&apos;s profile picture should not be irrelevant, abusive or vulgar. It
          should not be a default image provided by Enso.
        </span>
      </div>
    </div>
  )
}
