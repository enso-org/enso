/** @file Switcher to choose the currently visible assets table category. */
import * as React from 'react'

import CloudIcon from 'enso-assets/cloud.svg'
import ComputerIcon from 'enso-assets/computer.svg'
import PeopleIcon from 'enso-assets/people.svg'
import PersonIcon from 'enso-assets/person.svg'
import RecentIcon from 'enso-assets/recent.svg'
import Trash2Icon from 'enso-assets/trash2.svg'

import * as mimeTypes from '#/data/mimeTypes'

import * as backendHooks from '#/hooks/backendHooks'
import * as offlineHooks from '#/hooks/offlineHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'

import * as categoryModule from '#/layouts/CategorySwitcher/Category'
import type Category from '#/layouts/CategorySwitcher/Category'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import FocusArea from '#/components/styled/FocusArea'
import SvgMask from '#/components/SvgMask'

import * as backend from '#/services/Backend'

import * as tailwindMerge from '#/utilities/tailwindMerge'

// ========================
// === CategoryMetadata ===
// ========================

/** Metadata for a categoryModule.categoryType. */
interface CategoryMetadata {
  readonly isNested?: boolean
  readonly category: Category
  readonly icon: string
  readonly label: string
  readonly buttonLabel: string
  readonly dropZoneLabel: string
  readonly className?: string
  readonly iconClassName?: string
}

// ============================
// === CategorySwitcherItem ===
// ============================

/** Props for a {@link CategorySwitcherItem}. */
interface InternalCategorySwitcherItemProps extends CategoryMetadata {
  readonly currentCategory: Category
  readonly setCategory: (category: Category) => void
  readonly dispatchAssetEvent: (directoryEvent: assetEvent.AssetEvent) => void
}

/** An entry in a {@link CategorySwitcher}. */
function CategorySwitcherItem(props: InternalCategorySwitcherItemProps) {
  const { currentCategory, setCategory, dispatchAssetEvent } = props
  const { isNested = false, category, icon, label, buttonLabel, dropZoneLabel } = props
  const { iconClassName } = props
  const { user } = authProvider.useNonPartialUserSession()
  const { unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const localBackend = backendProvider.useLocalBackend()
  const { isOffline } = offlineHooks.useOffline()
  const isCurrent = categoryModule.areCategoriesEqual(currentCategory, category)
  const getCategoryError = (otherCategory: Category) => {
    switch (otherCategory.type) {
      case categoryModule.CategoryType.local: {
        if (localBackend == null) {
          return getText('localBackendNotDetectedError')
        } else {
          return null
        }
      }
      case categoryModule.CategoryType.cloud:
      case categoryModule.CategoryType.recent:
      case categoryModule.CategoryType.trash:
      case categoryModule.CategoryType.user:
      case categoryModule.CategoryType.team: {
        if (isOffline) {
          return getText('unavailableOffline')
        } else if (!user.isEnabled) {
          return getText('notEnabledSubtitle')
        } else {
          return null
        }
      }
    }
  }
  const error = getCategoryError(category)
  const isDisabled = error != null
  const tooltip = error ?? false

  const isDropTarget = (() => {
    if (categoryModule.areCategoriesEqual(category, currentCategory)) {
      return false
    } else if (currentCategory.type === categoryModule.CategoryType.trash) {
      switch (category.type) {
        case categoryModule.CategoryType.trash:
        case categoryModule.CategoryType.recent: {
          return false
        }
        default: {
          return true
        }
      }
    } else {
      return category.type !== categoryModule.CategoryType.recent
    }
  })()
  const acceptedDragTypes = isDropTarget ? [mimeTypes.ASSETS_MIME_TYPE] : []

  const onPress = () => {
    if (error == null && !categoryModule.areCategoriesEqual(category, currentCategory)) {
      setCategory(category)
    }
  }

  const onDrop = (event: aria.DropEvent) => {
    unsetModal()
    void Promise.all(
      event.items.flatMap(async item => {
        if (item.kind === 'text') {
          const text = await item.getText(mimeTypes.ASSETS_MIME_TYPE)
          const payload: unknown = JSON.parse(text)
          return Array.isArray(payload)
            ? payload.flatMap(key =>
                // This is SAFE, assuming only this app creates payloads with
                // the specific mimetype above.
                // eslint-disable-next-line no-restricted-syntax
                typeof key === 'string' ? [key as backend.AssetId] : []
              )
            : []
        } else {
          return []
        }
      })
    ).then(keys => {
      dispatchAssetEvent({
        type:
          currentCategory.type === categoryModule.CategoryType.trash
            ? AssetEventType.restore
            : AssetEventType.delete,
        ids: new Set(keys.flat(1)),
      })
    })
  }

  const element = (
    <aria.DropZone
      aria-label={dropZoneLabel}
      getDropOperation={types =>
        acceptedDragTypes.some(type => types.has(type)) ? 'move' : 'cancel'
      }
      className="group relative flex items-center rounded-full drop-target-after"
      onDrop={onDrop}
    >
      <ariaComponents.Button
        size="custom"
        variant="custom"
        tooltip={tooltip}
        tooltipPlacement="right"
        className={tailwindMerge.twMerge(
          isCurrent && 'focus-default',
          isDisabled && 'cursor-not-allowed hover:bg-transparent'
        )}
        aria-label={buttonLabel}
        onPress={onPress}
      >
        <div
          className={tailwindMerge.twMerge(
            'group flex h-row items-center gap-icon-with-text rounded-full px-button-x selectable',
            isCurrent && 'disabled active',
            !isCurrent && !isDisabled && 'hover:bg-selected-frame'
          )}
        >
          <SvgMask src={icon} className={iconClassName} />
          <aria.Text slot="description">{label}</aria.Text>
        </div>
      </ariaComponents.Button>
      <div className="absolute left-full ml-2 hidden group-focus-visible:block">
        {getText('drop')}
      </div>
    </aria.DropZone>
  )

  return isNested ? (
    <div className="flex">
      <div className="ml-[15px] mr-1 border-r border-primary/20" />
      {element}
    </div>
  ) : (
    element
  )
}

// ========================
// === CategorySwitcher ===
// ========================

/** Props for a {@link CategorySwitcher}. */
export interface CategorySwitcherProps {
  readonly category: Category
  readonly setCategory: (category: Category) => void
  readonly dispatchAssetEvent: (directoryEvent: assetEvent.AssetEvent) => void
}

/** A switcher to choose the currently visible assets table categoryModule.categoryType. */
export default function CategorySwitcher(props: CategorySwitcherProps) {
  const { category, setCategory, dispatchAssetEvent } = props
  const { user } = authProvider.useNonPartialUserSession()
  const { getText } = textProvider.useText()
  const remoteBackend = backendProvider.useRemoteBackend()
  const localBackend = backendProvider.useLocalBackend()
  const itemProps = { currentCategory: category, setCategory, dispatchAssetEvent }
  const selfDirectoryId = backend.DirectoryId(`directory-${user.userId.replace(/^user-/, '')}`)

  const usersDirectoryQuery = backendHooks.useBackendQuery(remoteBackend, 'listDirectory', [
    {
      parentId: backend.DirectoryId('directory-0000000000000000000000teams'),
      filterBy: backend.FilterBy.active,
      labels: [],
      recentProjects: false,
    },
    'Users',
  ])
  const teamsDirectoryQuery = backendHooks.useBackendQuery(remoteBackend, 'listDirectory', [
    {
      parentId: backend.DirectoryId('directory-0000000000000000000000teams'),
      filterBy: backend.FilterBy.active,
      labels: [],
      recentProjects: false,
    },

    'Teams',
  ])

  return (
    <FocusArea direction="vertical">
      {innerProps => (
        <div className="flex w-full flex-col gap-2 py-1" {...innerProps}>
          <ariaComponents.Text variant="subtitle" className="px-2 font-bold">
            {getText('category')}
          </ariaComponents.Text>

          <div
            aria-label={getText('categorySwitcherMenuLabel')}
            role="grid"
            className="flex flex-col items-start"
          >
            <CategorySwitcherItem
              {...itemProps}
              category={{ type: categoryModule.CategoryType.cloud }}
              icon={CloudIcon}
              label={getText('cloudCategory')}
              buttonLabel={getText('cloudCategoryButtonLabel')}
              dropZoneLabel={getText('cloudCategoryDropZoneLabel')}
            />
            {(user.plan === backend.Plan.team || user.plan === backend.Plan.enterprise) && (
              <CategorySwitcherItem
                {...itemProps}
                isNested
                category={{
                  type: categoryModule.CategoryType.user,
                  rootPath: backend.Path(`enso://Users/${user.name}`),
                  homeDirectoryId: selfDirectoryId,
                }}
                icon={PersonIcon}
                label={getText('myFilesCategory')}
                buttonLabel={getText('myFilesCategoryButtonLabel')}
                dropZoneLabel={getText('myFilesCategoryDropZoneLabel')}
              />
            )}
            <CategorySwitcherItem
              {...itemProps}
              isNested
              category={{ type: categoryModule.CategoryType.recent }}
              icon={RecentIcon}
              label={getText('recentCategory')}
              buttonLabel={getText('recentCategoryButtonLabel')}
              dropZoneLabel={getText('recentCategoryDropZoneLabel')}
              iconClassName="-ml-0.5"
            />
            <CategorySwitcherItem
              {...itemProps}
              isNested
              category={{ type: categoryModule.CategoryType.trash }}
              icon={Trash2Icon}
              label={getText('trashCategory')}
              buttonLabel={getText('trashCategoryButtonLabel')}
              dropZoneLabel={getText('trashCategoryDropZoneLabel')}
            />
            {usersDirectoryQuery.data?.map(userDirectory =>
              userDirectory.id === selfDirectoryId ? null : (
                <CategorySwitcherItem
                  key={userDirectory.id}
                  {...itemProps}
                  isNested
                  category={{
                    type: categoryModule.CategoryType.user,
                    rootPath: backend.Path(`enso://Users/${userDirectory.title}`),
                    // This is SAFE as user directories are guaranteed to be directories.
                    // eslint-disable-next-line no-restricted-syntax
                    homeDirectoryId: userDirectory.id as backend.DirectoryId,
                  }}
                  icon={PersonIcon}
                  label={getText('userCategory', userDirectory.title)}
                  buttonLabel={getText('userCategoryButtonLabel', userDirectory.title)}
                  dropZoneLabel={getText('userCategoryDropZoneLabel', userDirectory.title)}
                />
              )
            )}
            {teamsDirectoryQuery.data?.map(teamDirectory => (
              <CategorySwitcherItem
                key={teamDirectory.id}
                {...itemProps}
                isNested
                category={{
                  type: categoryModule.CategoryType.team,
                  rootPath: backend.Path(`enso://Teams/${teamDirectory.title}`),
                  // This is SAFE as team directories are guaranteed to be directories.
                  // eslint-disable-next-line no-restricted-syntax
                  homeDirectoryId: teamDirectory.id as backend.DirectoryId,
                }}
                icon={PeopleIcon}
                label={getText('teamCategory', teamDirectory.title)}
                buttonLabel={getText('teamCategoryButtonLabel', teamDirectory.title)}
                dropZoneLabel={getText('teamCategoryDropZoneLabel', teamDirectory.title)}
              />
            ))}
            {localBackend != null && (
              <CategorySwitcherItem
                {...itemProps}
                category={{ type: categoryModule.CategoryType.local }}
                icon={ComputerIcon}
                label={getText('localCategory')}
                buttonLabel={getText('localCategoryButtonLabel')}
                dropZoneLabel={getText('localCategoryDropZoneLabel')}
              />
            )}
          </div>
        </div>
      )}
    </FocusArea>
  )
}
