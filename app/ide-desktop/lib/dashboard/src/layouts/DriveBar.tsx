/** @file Header menubar for the directory listing, containing information about
 * the current directory and some configuration options. */
import * as React from 'react'

import AddDatalinkIcon from 'enso-assets/add_datalink.svg'
import AddFolderIcon from 'enso-assets/add_folder.svg'
import AddKeyIcon from 'enso-assets/add_key.svg'
import DataDownloadIcon from 'enso-assets/data_download.svg'
import DataUploadIcon from 'enso-assets/data_upload.svg'
import RightPanelIcon from 'enso-assets/right_panel.svg'

import * as offlineHooks from '#/hooks/offlineHooks'

import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import AssetEventType from '#/events/AssetEventType'

import type * as assetSearchBar from '#/layouts/AssetSearchBar'
import AssetSearchBar from '#/layouts/AssetSearchBar'
import * as eventListProvider from '#/layouts/AssetsTable/EventListProvider'
import Category, * as categoryModule from '#/layouts/CategorySwitcher/Category'
import StartModal from '#/layouts/StartModal'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'

import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import UpsertDatalinkModal from '#/modals/UpsertDatalinkModal'
import UpsertSecretModal from '#/modals/UpsertSecretModal'

import type Backend from '#/services/Backend'

import type AssetQuery from '#/utilities/AssetQuery'
import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'

// ================
// === DriveBar ===
// ================

/** Props for a {@link DriveBar}. */
export interface DriveBarProps {
  readonly backend: Backend
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly suggestions: readonly assetSearchBar.Suggestion[]
  readonly category: Category
  readonly canDownload: boolean
  readonly isAssetPanelOpen: boolean
  readonly setIsAssetPanelOpen: React.Dispatch<React.SetStateAction<boolean>>
  readonly doEmptyTrash: () => void
  readonly doCreateProject: () => void
  readonly doCreateDirectory: () => void
  readonly doCreateSecret: (name: string, value: string) => void
  readonly doCreateDatalink: (name: string, value: unknown) => void
  readonly doUploadFiles: (files: File[]) => void
}

/** Displays the current directory path and permissions, upload and download buttons,
 * and a column display mode switcher. */
export default function DriveBar(props: DriveBarProps) {
  const { backend, query, setQuery, suggestions, category, canDownload } = props
  const { doEmptyTrash, doCreateProject, doCreateDirectory } = props
  const { doCreateSecret, doCreateDatalink, doUploadFiles } = props
  const { isAssetPanelOpen, setIsAssetPanelOpen } = props
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const dispatchAssetEvent = eventListProvider.useDispatchAssetEvent()
  const uploadFilesRef = React.useRef<HTMLInputElement>(null)
  const isCloud = categoryModule.isCloud(category)
  const { isOffline } = offlineHooks.useOffline()

  const shouldBeDisabled = isCloud && isOffline

  React.useEffect(() => {
    return inputBindings.attach(sanitizedEventTargets.document.body, 'keydown', {
      ...(isCloud
        ? {
            newFolder: () => {
              doCreateDirectory()
            },
          }
        : {}),
      newProject: () => {
        doCreateProject()
      },
      uploadFiles: () => {
        uploadFilesRef.current?.click()
      },
    })
  }, [isCloud, doCreateDirectory, doCreateProject, inputBindings])

  const searchBar = (
    <AssetSearchBar
      backend={backend}
      isCloud={isCloud}
      query={query}
      setQuery={setQuery}
      suggestions={suggestions}
    />
  )

  const assetPanelToggle = (
    <>
      {/* Spacing. */}
      <div className={!isAssetPanelOpen ? 'w-5' : 'hidden'} />
      <div className="absolute right-[15px] top-[27px] z-1">
        <ariaComponents.Button
          size="medium"
          variant="custom"
          isActive={isAssetPanelOpen}
          icon={RightPanelIcon}
          aria-label={isAssetPanelOpen ? getText('openAssetPanel') : getText('closeAssetPanel')}
          onPress={() => {
            setIsAssetPanelOpen(isOpen => !isOpen)
          }}
        />
      </div>
    </>
  )

  switch (category) {
    case Category.recent: {
      return (
        <ariaComponents.ButtonGroup className="my-0.5 grow-0">
          {searchBar}
          {assetPanelToggle}
        </ariaComponents.ButtonGroup>
      )
    }
    case Category.trash: {
      return (
        <ariaComponents.ButtonGroup className="my-0.5 grow-0">
          <ariaComponents.Button
            size="medium"
            variant="bar"
            isDisabled={shouldBeDisabled}
            onPress={() => {
              setModal(
                <ConfirmDeleteModal
                  actionText={getText('allTrashedItemsForever')}
                  doDelete={doEmptyTrash}
                />
              )
            }}
          >
            {getText('clearTrash')}
          </ariaComponents.Button>
          {searchBar}
          {assetPanelToggle}
        </ariaComponents.ButtonGroup>
      )
    }
    case Category.cloud:
    case Category.local: {
      return (
        <ariaComponents.ButtonGroup className="my-0.5 grow-0">
          <aria.DialogTrigger>
            <ariaComponents.Button size="medium" variant="tertiary" isDisabled={shouldBeDisabled}>
              {getText('startWithATemplate')}
            </ariaComponents.Button>

            <StartModal createProject={doCreateProject} />
          </aria.DialogTrigger>
          <ariaComponents.Button
            size="medium"
            variant="bar"
            isDisabled={shouldBeDisabled}
            onPress={() => {
              doCreateProject()
            }}
          >
            {getText('newEmptyProject')}
          </ariaComponents.Button>
          <div className="flex h-row items-center gap-4 rounded-full border-0.5 border-primary/20 px-[11px] text-primary/50">
            <ariaComponents.Button
              variant="icon"
              size="medium"
              icon={AddFolderIcon}
              isDisabled={shouldBeDisabled}
              aria-label={getText('newFolder')}
              onPress={() => {
                doCreateDirectory()
              }}
            />
            {isCloud && (
              <ariaComponents.Button
                variant="icon"
                size="medium"
                icon={AddKeyIcon}
                isDisabled={shouldBeDisabled}
                aria-label={getText('newSecret')}
                onPress={() => {
                  setModal(<UpsertSecretModal id={null} name={null} doCreate={doCreateSecret} />)
                }}
              />
            )}
            {isCloud && (
              <ariaComponents.Button
                variant="icon"
                size="medium"
                icon={AddDatalinkIcon}
                isDisabled={shouldBeDisabled}
                aria-label={getText('newDatalink')}
                onPress={() => {
                  setModal(<UpsertDatalinkModal doCreate={doCreateDatalink} />)
                }}
              />
            )}
            <aria.Input
              ref={uploadFilesRef}
              type="file"
              multiple
              className="hidden"
              onInput={event => {
                if (event.currentTarget.files != null) {
                  doUploadFiles(Array.from(event.currentTarget.files))
                }
                // Clear the list of selected files. Otherwise, `onInput` will not be
                // dispatched again if the same file is selected.
                event.currentTarget.value = ''
              }}
            />
            <ariaComponents.Button
              variant="icon"
              size="medium"
              icon={DataUploadIcon}
              isDisabled={shouldBeDisabled}
              aria-label={getText('uploadFiles')}
              onPress={() => {
                unsetModal()
                uploadFilesRef.current?.click()
              }}
            />
            <ariaComponents.Button
              isDisabled={!canDownload || shouldBeDisabled}
              variant="icon"
              size="medium"
              icon={DataDownloadIcon}
              aria-label={getText('downloadFiles')}
              onPress={() => {
                unsetModal()
                dispatchAssetEvent({ type: AssetEventType.downloadSelected })
              }}
            />
          </div>
          {searchBar}
          {assetPanelToggle}
        </ariaComponents.ButtonGroup>
      )
    }
  }
}
