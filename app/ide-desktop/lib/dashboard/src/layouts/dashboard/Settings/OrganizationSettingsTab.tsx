/** @file Settings tab for viewing and editing account information. */
import * as React from 'react'

import DefaultUserIcon from 'enso-assets/default_user.svg'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'

import * as backendModule from '#/services/Backend'

import ConfirmDeleteUserModal from '../ConfirmDeleteUserModal'

// =================
// === InfoEntry ===
// =================

/** Props for a transparent wrapper component. */
interface InternalTransparentWrapperProps {
  readonly children: React.ReactNode
}

/** A transparent wrapper component */
// This is a React component even though it does not contain JSX.
// eslint-disable-next-line no-restricted-syntax
function Name(props: InternalTransparentWrapperProps) {
  return props.children
}

/** A transparent wrapper component */
// This is a React component even though it does not contain JSX.
// eslint-disable-next-line no-restricted-syntax
function Value(props: InternalTransparentWrapperProps) {
  return props.children
}

/** Props for a {@link InfoEntry}. */
interface InternalInfoEntryProps {
  readonly children: [React.ReactNode, React.ReactNode]
}

/** Styled information display containing key and value. */
function InfoEntry(props: InternalInfoEntryProps) {
  const { children } = props
  const [name, value] = children
  return (
    <div className="flex gap-4.75">
      <span className="leading-5 w-40 h-8 py-1.25">{name}</span>
      <span className="grow font-bold leading-5 h-8 py-1.25">{value}</span>
    </div>
  )
}

// ===============================
// === OrganizationSettingsTab ===
// ===============================

/** Props for a {@link OrganizationSettingsTab}. */
export interface OrganizationSettingsTabProps {
  readonly organization: backendModule.OrganizationInfo | null
  readonly setOrganization: (organization: backendModule.OrganizationInfo) => void
}

/** Settings tab for viewing and editing organization information. */
export default function OrganizationSettingsTab(props: OrganizationSettingsTabProps) {
  const { organization, setOrganization } = props
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { signOut } = authProvider.useAuth()
  const { setModal } = modalProvider.useSetModal()
  const { backend } = backendProvider.useBackend()
  const inputRefs: Record<
    Exclude<keyof backendModule.OrganizationInfo, 'profilePicture'>,
    React.RefObject<HTMLInputElement>
  > = {
    name: React.useRef<HTMLInputElement>(null),
    email: React.useRef<HTMLInputElement>(null),
    location: React.useRef<HTMLInputElement>(null),
    website: React.useRef<HTMLInputElement>(null),
  }

  const doUpdateOrganization = async <
    K extends Exclude<keyof backendModule.OrganizationInfo, 'profilePicture'>,
  >(
    key: K,
    value: backendModule.OrganizationInfo[K]
  ) => {
    if (organization != null) {
      const oldValue = organization[key]
      const newValue = inputRefs[key].current?.value ?? ''
      if (newValue === oldValue) {
        return
      } else {
        try {
          await backend.updateOrganization({ [key]: value })
        } catch (error) {
          toastAndLog(null, error)
          const ref = inputRefs[key].current
          if (ref) {
            ref.value = oldValue
          }
        }
        return
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
    <div className="flex gap-8">
      <div className="flex flex-col gap-8">
        <div className="flex flex-col gap-2.5">
          <h3 className="font-bold text-xl h-9.5 py-0.5">Organization</h3>
          <div className="flex flex-col">
            <InfoEntry>
              <Name>Organization display name</Name>
              <Value>
                <input
                  ref={inputRefs.name}
                  className="rounded-full font-bold leading-5 w-full h-8 -mx-2 -my-1.25 px-2 py-1.25 bg-transparent hover:bg-frame-selected focus:bg-frame-selected transition-colors"
                  key={organization?.name}
                  type="text"
                  size={1}
                  defaultValue={organization?.name ?? ''}
                  onBlur={event => {
                    void doUpdateOrganization('name', event.currentTarget.value)
                  }}
                  onKeyDown={event => {
                    onKeyDown(event, organization?.name ?? '')
                  }}
                />
              </Value>
            </InfoEntry>
            <InfoEntry>
              <Name>Email</Name>
              <Value>
                <input
                  ref={inputRefs.email}
                  className="rounded-full font-bold leading-5 w-full h-8 -mx-2 -my-1.25 px-2 py-1.25 bg-transparent hover:bg-frame-selected focus:bg-frame-selected transition-colors"
                  key={organization?.email}
                  type="text"
                  size={1}
                  defaultValue={organization?.email ?? ''}
                  onBlur={event => {
                    void doUpdateOrganization(
                      'email',
                      backendModule.EmailAddress(event.currentTarget.value)
                    )
                  }}
                  onKeyDown={event => {
                    onKeyDown(event, organization?.email ?? '')
                  }}
                />
              </Value>
            </InfoEntry>
            <InfoEntry>
              <Name>Website</Name>
              <Value>
                <input
                  ref={inputRefs.website}
                  className="rounded-full font-bold leading-5 w-full h-8 -mx-2 -my-1.25 px-2 py-1.25 bg-transparent hover:bg-frame-selected focus:bg-frame-selected transition-colors"
                  key={organization?.website}
                  type="text"
                  size={1}
                  defaultValue={organization?.website ?? ''}
                  onBlur={event => {
                    void doUpdateOrganization('website', event.currentTarget.value)
                  }}
                  onKeyDown={event => {
                    onKeyDown(event, organization?.website ?? '')
                  }}
                />
              </Value>
            </InfoEntry>
            <InfoEntry>
              <Name>Location</Name>
              <Value>
                <input
                  ref={inputRefs.location}
                  className="rounded-full font-bold leading-5 w-full h-8 -mx-2 -my-1.25 px-2 py-1.25 bg-transparent hover:bg-frame-selected focus:bg-frame-selected transition-colors"
                  key={organization?.location}
                  type="text"
                  size={1}
                  defaultValue={organization?.location ?? ''}
                  onBlur={event => {
                    void doUpdateOrganization('location', event.currentTarget.value)
                  }}
                  onKeyDown={event => {
                    onKeyDown(event, organization?.location ?? '')
                  }}
                />
              </Value>
            </InfoEntry>
          </div>
        </div>
        <div className="flex flex-col gap-2.5 rounded-2.5xl border-2 border-danger px-4 pt-2.25 pb-3.75">
          <h3 className="text-danger font-bold text-xl h-9.5 py-0.5">Danger Zone</h3>
          <div className="flex gap-2">
            <button
              className="rounded-full bg-danger text-inversed px-2 py-1"
              onClick={event => {
                event.stopPropagation()
                setModal(
                  <ConfirmDeleteUserModal
                    description="organization"
                    doDelete={async () => {
                      await backend.deleteOrganization()
                      await signOut()
                    }}
                  />
                )
              }}
            >
              <span className="leading-5 h-6 py-px">Delete this organization</span>
            </button>
            <span className="leading-5 h-8 py-1.25">
              Once deleted, it will be gone forever. Please be certain.
            </span>
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
          <img src={organization?.profilePicture ?? DefaultUserIcon} width={128} height={128} />
        </label>
        <span className="py-1 w-64">
          Your organization&apos;s profile picture should not be irrelevant, abusive or vulgar. It
          should not be a default image provided by Enso.
        </span>
      </div>
    </div>
  )
}
