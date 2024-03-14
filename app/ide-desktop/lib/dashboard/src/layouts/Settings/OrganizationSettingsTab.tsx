/** @file Settings tab for viewing and editing account information. */
import * as React from 'react'

import isEmail from 'validator/lib/isEmail'

import DefaultUserIcon from 'enso-assets/default_user.svg'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as backendModule from '#/services/Backend'

import * as object from '#/utilities/object'

// ===============================
// === OrganizationSettingsTab ===
// ===============================

/** Props for a {@link OrganizationSettingsTab}. */
export interface OrganizationSettingsTabProps {
  readonly organization: backendModule.SmartOrganization | null
  readonly setOrganizationInfo: React.Dispatch<React.SetStateAction<backendModule.OrganizationInfo>>
}

/** Settings tab for viewing and editing organization information. */
export default function OrganizationSettingsTab(props: OrganizationSettingsTabProps) {
  const { organization, setOrganizationInfo } = props
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const nameRef = React.useRef<HTMLInputElement>(null)
  const emailRef = React.useRef<HTMLInputElement>(null)
  const websiteRef = React.useRef<HTMLInputElement>(null)
  const locationRef = React.useRef<HTMLInputElement>(null)

  const doUpdateName = async () => {
    const oldName = organization?.value?.organization_name ?? null
    const name = nameRef.current?.value ?? ''
    if (oldName !== name) {
      try {
        // eslint-disable-next-line @typescript-eslint/naming-convention
        setOrganizationInfo(object.merger({ organization_name: name }))
        const newOrganization = await organization?.update({ name })
        if (newOrganization != null) {
          setOrganizationInfo(newOrganization)
        }
      } catch (error) {
        // eslint-disable-next-line @typescript-eslint/naming-convention
        setOrganizationInfo(object.merger({ organization_name: oldName }))
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
        const newOrganization = await organization?.update({ location })
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

  const doUploadOrganizationPicture = async (event: React.ChangeEvent<HTMLInputElement>) => {
    const image = event.target.files?.[0]
    if (image == null) {
      toastAndLog('Could not upload a new profile picture because no image was found')
    } else {
      try {
        const newOrganization = await organization?.uploadPicture({ fileName: image.name }, image)
        if (newOrganization != null) {
          setOrganizationInfo(newOrganization)
        }
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
                  key={organization?.value?.organization_name}
                  type="text"
                  size={1}
                  defaultValue={organization?.value?.organization_name ?? ''}
                  onBlur={doUpdateName}
                  onKeyDown={event => {
                    onKeyDown(event, organization?.value?.organization_name ?? '')
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
                  key={organization?.value?.email}
                  type="text"
                  size={1}
                  defaultValue={organization?.value?.email ?? ''}
                  onBlur={event => {
                    if (isEmail(event.currentTarget.value)) {
                      void doUpdateEmail()
                    } else {
                      event.currentTarget.focus()
                    }
                  }}
                  onKeyDown={event => {
                    onKeyDown(event, organization?.value?.email ?? '')
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
                  key={organization?.value?.website}
                  type="text"
                  size={1}
                  defaultValue={organization?.value?.website ?? ''}
                  onBlur={doUpdateWebsite}
                  onKeyDown={event => {
                    onKeyDown(event, organization?.value?.website ?? '')
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
                  key={organization?.value?.address}
                  type="text"
                  size={1}
                  defaultValue={organization?.value?.address ?? ''}
                  onBlur={doUpdateLocation}
                  onKeyDown={event => {
                    onKeyDown(event, organization?.value?.address ?? '')
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
            src={organization?.value?.picture ?? DefaultUserIcon}
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
