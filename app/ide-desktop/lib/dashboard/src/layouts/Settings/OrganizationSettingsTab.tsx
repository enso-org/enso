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
    <div className="flex flex-col lg:flex-row gap-8 h-0 lg:h-auto">
      <div className="flex flex-col gap-8 w-120">
        <div className="flex flex-col gap-2.5">
          <h3 className="font-bold text-xl h-9.5 py-0.5">Organization</h3>
          <div className="flex flex-col">
            <div className="flex gap-4.75">
              <span className="leading-5 w-40 h-8 py-1.25">Organization display name</span>
              <span className="grow font-bold leading-5 h-8 py-1.25">
                <input
                  ref={nameRef}
                  className="rounded-full font-bold leading-5 w-full h-8 -mx-2 -my-1.25 px-2 py-1.25 bg-transparent hover:bg-frame-selected focus:bg-frame-selected transition-colors"
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
            <div className="flex gap-4.75">
              <span className="leading-5 w-40 h-8 py-1.25">Organization display name</span>
              <span className="grow font-bold leading-5 h-8 py-1.25">
                <input
                  ref={nameRef}
                  className="rounded-full font-bold leading-5 w-full h-8 -mx-2 -my-1.25 px-2 py-1.25 bg-transparent hover:bg-frame-selected focus:bg-frame-selected transition-colors"
                  key={organization?.value?.organization_name ?? ''}
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
            <div className="flex gap-4.75">
              <span className="leading-5 w-40 h-8 py-1.25">Email</span>
              <span className="grow font-bold leading-5 h-8 py-1.25">
                <input
                  ref={emailRef}
                  className="rounded-full font-bold leading-5 w-full h-8 -mx-2 -my-1.25 px-2 py-1.25 bg-transparent hover:bg-frame-selected focus:bg-frame-selected transition-colors invalid:border invalid:border-red-700"
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
            <div className="flex gap-4.75">
              <span className="leading-5 w-40 h-8 py-1.25">Website</span>
              <span className="grow font-bold leading-5 h-8 py-1.25">
                <input
                  ref={websiteRef}
                  className="rounded-full font-bold leading-5 w-full h-8 -mx-2 -my-1.25 px-2 py-1.25 bg-transparent hover:bg-frame-selected focus:bg-frame-selected transition-colors"
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
            <div className="flex gap-4.75">
              <span className="leading-5 w-40 h-8 py-1.25">Location</span>
              <span className="grow font-bold leading-5 h-8 py-1.25">
                <input
                  ref={locationRef}
                  className="rounded-full font-bold leading-5 w-full h-8 -mx-2 -my-1.25 px-2 py-1.25 bg-transparent hover:bg-frame-selected focus:bg-frame-selected transition-colors"
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
      <div className="flex flex-col gap-2.5">
        <h3 className="font-bold text-xl h-9.5 py-0.5">Profile picture</h3>
        <label className="flex items-center cursor-pointer rounded-full overflow-clip h-32 w-32 hover:bg-frame transition-colors">
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
        <span className="py-1 w-64">
          Your organization&apos;s profile picture should not be irrelevant, abusive or vulgar. It
          should not be a default image provided by Enso.
        </span>
      </div>
    </div>
  )
}
