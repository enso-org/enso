/** @file Switcher to choose the currently visible assets table category. */
import * as React from 'react'

import { useSearchParams } from 'react-router-dom'

import { SEARCH_PARAMS_PREFIX } from '#/appUtils'
import FolderAddIcon from '#/assets/folder_add.svg'
import Minus2Icon from '#/assets/minus2.svg'
import SettingsIcon from '#/assets/settings.svg'
import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import { Badge } from '#/components/Badge'
import * as mimeTypes from '#/data/mimeTypes'
import * as offlineHooks from '#/hooks/offlineHooks'
import {
  areCategoriesEqual,
  canTransferBetweenCategories,
  useTransferBetweenCategories,
  type Category,
} from '#/layouts/CategorySwitcher/Category'
import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import { TabType } from '#/providers/ProjectsProvider'
import * as textProvider from '#/providers/TextProvider'
import type * as backend from '#/services/Backend'
import { tv } from '#/utilities/tailwindVariants'
import { twJoin } from 'tailwind-merge'
import { AnimatedBackground } from '../components/AnimatedBackground'
import { useEventCallback } from '../hooks/eventCallbackHooks'

import { useCloudCategoryList, useLocalCategoryList } from './Drive/Categories/categoriesHooks'

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

/** Props for a {@link CategorySwitcherItem}. */
interface InternalCategorySwitcherItemProps extends CategoryMetadata {
  readonly currentCategory: Category
  readonly setCategoryId: (categoryId: Category['id']) => void
  readonly badgeContent?: React.ReactNode
  readonly isDisabled: boolean
}

const CATEGORY_SWITCHER_VARIANTS = tv({
  extend: ariaComponents.BUTTON_STYLES,
  base: 'group opacity-50 transition-opacity group-hover:opacity-100 w-auto max-w-full',
  slots: {
    wrapper: 'w-full',
    text: 'flex-1 min-w-0 w-auto items-start justify-start',
  },
})

/** An entry in a {@link CategorySwitcher}. */
function CategorySwitcherItem(props: InternalCategorySwitcherItemProps) {
  const { currentCategory, setCategoryId, badgeContent, isDisabled: isDisabledRaw } = props
  const { isNested = false, category, icon, label, buttonLabel, dropZoneLabel } = props

  const [isTransitioning, startTransition] = React.useTransition()

  const { user } = authProvider.useFullUserSession()
  const { unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const localBackend = backendProvider.useLocalBackend()
  const { isOffline } = offlineHooks.useOffline()

  const isCurrent = areCategoriesEqual(currentCategory, category)

  const transferBetweenCategories = useTransferBetweenCategories(currentCategory)

  const getCategoryError = useEventCallback((otherCategory: Category) => {
    switch (otherCategory.type) {
      case 'local':
      case 'local-directory': {
        if (localBackend == null) {
          return getText('localBackendNotDetectedError')
        } else {
          return null
        }
      }
      case 'cloud':
      case 'recent':
      case 'trash':
      case 'user':
      case 'team': {
        if (isOffline) {
          return getText('unavailableOffline')
        } else if (!user.isEnabled) {
          return getText('notEnabledSubtitle')
        } else {
          return null
        }
      }
    }
  })
  const error = getCategoryError(category)
  const isDisabled = error != null || isDisabledRaw
  const tooltip = error ?? false

  const isDropTarget =
    !areCategoriesEqual(currentCategory, category) &&
    canTransferBetweenCategories(currentCategory, category, user)
  const acceptedDragTypes = isDropTarget ? [mimeTypes.ASSETS_MIME_TYPE] : []

  const onPress = useEventCallback(() => {
    if (error == null && !areCategoriesEqual(category, currentCategory)) {
      // We use startTransition to trigger a background transition between categories.
      // and to not invoke the Suspense boundary.
      // This makes the transition feel more responsive and natural.
      startTransition(() => {
        setCategoryId(category.id)
      })
    }
  })

  const onDrop = useEventCallback((event: aria.DropEvent) => {
    unsetModal()
    void Promise.all(
      event.items.flatMap(async (item) => {
        if (item.kind === 'text') {
          const text = await item.getText(mimeTypes.ASSETS_MIME_TYPE)
          const payload: unknown = JSON.parse(text)
          return Array.isArray(payload) ?
              payload.flatMap((key) =>
                // This is SAFE, assuming only this app creates payloads with
                // the specific mimetype above.
                // eslint-disable-next-line no-restricted-syntax
                typeof key === 'string' ? [key as backend.AssetId] : [],
              )
            : []
        } else {
          return []
        }
      }),
    ).then((keys) => {
      transferBetweenCategories(currentCategory, category, keys.flat(1))
    })
  })

  const element = (
    <aria.DropZone
      aria-label={dropZoneLabel}
      getDropOperation={(types) =>
        acceptedDragTypes.some((type) => types.has(type)) ? 'move' : 'cancel'
      }
      className="group relative flex w-full min-w-0 flex-auto items-start rounded-full drop-target-after"
      onDrop={onDrop}
    >
      <AnimatedBackground.Item
        isSelected={isCurrent}
        className="w-auto max-w-[calc(100%-24px)]"
        animationClassName="bg-invert rounded-full"
      >
        <ariaComponents.Button
          size="medium"
          variant="custom"
          tooltip={tooltip}
          tooltipPlacement="right"
          variants={CATEGORY_SWITCHER_VARIANTS}
          isDisabled={isDisabled}
          aria-label={buttonLabel}
          onPress={onPress}
          loaderPosition="icon"
          data-selected={isCurrent}
          loading={isTransitioning}
          className={twJoin(isCurrent && 'opacity-100')}
          icon={icon}
          addonEnd={
            badgeContent != null && (
              <Badge color="accent" variant="solid">
                {badgeContent}
              </Badge>
            )
          }
        >
          <ariaComponents.Text
            disableLineHeightCompensation
            weight="semibold"
            color="current"
            truncate="1"
          >
            {label}
          </ariaComponents.Text>
        </ariaComponents.Button>
      </AnimatedBackground.Item>
      <div className="absolute left-full ml-2 hidden group-focus-visible:block">
        {getText('drop')}
      </div>
    </aria.DropZone>
  )

  return isNested ?
      <div className="flex w-full min-w-0 max-w-full flex-1">
        <div className="ml-[15px] mr-1.5 rounded-full border-r border-primary/20" />
        {element}
      </div>
    : element
}

/** Props for a {@link CategorySwitcher}. */
export interface CategorySwitcherProps {
  readonly category: Category
  readonly setCategoryId: (categoryId: Category['id']) => void
}

/** A switcher to choose the currently visible assets table categoryModule.categoryType. */
function CategorySwitcher(props: CategorySwitcherProps) {
  const { category, setCategoryId } = props

  const { getText } = textProvider.useText()
  const [, setSearchParams] = useSearchParams()

  const { isOffline } = offlineHooks.useOffline()

  const cloudCategories = useCloudCategoryList()
  const localCategories = useLocalCategoryList()

  const itemProps = { currentCategory: category, setCategoryId }

  const { cloudCategory, recentCategory, trashCategory, userCategory, teamCategories } =
    cloudCategories
  const { localCategory, directories, addDirectory, removeDirectory } = localCategories

  return (
    <div className="flex flex-col gap-2 py-1">
      <AnimatedBackground>
        <ariaComponents.Text variant="subtitle" weight="semibold" className="px-2">
          {getText('category')}
        </ariaComponents.Text>

        <div
          aria-label={getText('categorySwitcherMenuLabel')}
          role="grid"
          className="flex flex-col items-start"
        >
          <CategorySwitcherItem
            {...itemProps}
            key={cloudCategory.id}
            category={cloudCategory}
            icon={cloudCategory.icon}
            label={cloudCategory.label}
            isDisabled={isOffline}
            buttonLabel={getText('cloudCategoryButtonLabel')}
            dropZoneLabel={getText('cloudCategoryDropZoneLabel')}
            badgeContent={getText('cloudCategoryBadgeContent')}
          />

          {/* Self user space */}
          {userCategory != null && (
            <CategorySwitcherItem
              {...itemProps}
              isNested
              category={userCategory}
              icon={userCategory.icon}
              label={userCategory.label}
              isDisabled={isOffline}
              buttonLabel={getText('myFilesCategoryButtonLabel')}
              dropZoneLabel={getText('myFilesCategoryDropZoneLabel')}
            />
          )}

          {teamCategories.map((teamCategory) => (
            <CategorySwitcherItem
              key={teamCategory.id}
              {...itemProps}
              isNested
              category={teamCategory}
              icon={teamCategory.icon}
              label={teamCategory.label}
              isDisabled={isOffline}
              buttonLabel={getText('teamCategoryButtonLabel', teamCategory.team.name)}
              dropZoneLabel={getText('teamCategoryDropZoneLabel', teamCategory.team.name)}
            />
          ))}

          <CategorySwitcherItem
            {...itemProps}
            key={recentCategory.id}
            isNested
            category={recentCategory}
            icon={recentCategory.icon}
            label={recentCategory.label}
            isDisabled={isOffline}
            buttonLabel={getText('recentCategoryButtonLabel')}
            dropZoneLabel={getText('recentCategoryDropZoneLabel')}
          />

          <CategorySwitcherItem
            {...itemProps}
            key={trashCategory.id}
            isNested
            category={trashCategory}
            icon={trashCategory.icon}
            label={trashCategory.label}
            isDisabled={isOffline}
            buttonLabel={getText('trashCategoryButtonLabel')}
            dropZoneLabel={getText('trashCategoryDropZoneLabel')}
          />

          {localCategory != null && (
            <div className="group flex items-center gap-2 self-stretch drop-target-after">
              <CategorySwitcherItem
                {...itemProps}
                category={localCategory}
                icon={localCategory.icon}
                label={localCategory.label}
                isDisabled={false}
                buttonLabel={getText('localCategoryButtonLabel')}
                dropZoneLabel={getText('localCategoryDropZoneLabel')}
              />

              <ariaComponents.Button
                size="medium"
                variant="icon"
                extraClickZone="small"
                icon={SettingsIcon}
                aria-label={getText('changeLocalRootDirectoryInSettings')}
                className="my-auto opacity-0 transition-opacity group-hover:opacity-100"
                onPress={() => {
                  setSearchParams({
                    [`${SEARCH_PARAMS_PREFIX}SettingsTab`]: JSON.stringify('local'),
                    [`${SEARCH_PARAMS_PREFIX}page`]: JSON.stringify(TabType.settings),
                  })
                }}
              />
            </div>
          )}
          {directories != null &&
            directories.map((directory) => (
              <div key={directory.id} className="group flex items-center gap-2 self-stretch">
                <CategorySwitcherItem
                  {...itemProps}
                  isNested
                  category={directory}
                  icon={directory.icon}
                  label={directory.label}
                  isDisabled={false}
                  buttonLabel={getText('localCategoryButtonLabel')}
                  dropZoneLabel={getText('localCategoryDropZoneLabel')}
                />

                <ariaComponents.DialogTrigger>
                  <ariaComponents.Button
                    size="medium"
                    variant="icon"
                    extraClickZone={false}
                    icon={Minus2Icon}
                    aria-label={getText('removeDirectoryFromFavorites')}
                    className="hidden group-hover:block"
                  />

                  <ConfirmDeleteModal
                    actionText={getText('removeTheLocalDirectoryXFromFavorites', directory.label)}
                    actionButtonLabel={getText('remove')}
                    doDelete={async () => {
                      removeDirectory(directory.id)
                      await Promise.resolve()
                    }}
                  />
                </ariaComponents.DialogTrigger>
              </div>
            ))}

          {directories != null && window.fileBrowserApi && (
            <div className="flex">
              <div className="ml-[15px] mr-1.5 rounded-full border-r border-primary/20" />

              <ariaComponents.Button
                size="medium"
                variant="icon"
                icon={FolderAddIcon}
                loaderPosition="icon"
                onPress={async () => {
                  const [newDirectory] =
                    (await window.fileBrowserApi?.openFileBrowser('directory')) ?? []
                  if (newDirectory != null) {
                    addDirectory(newDirectory)
                  }
                }}
              >
                {getText('addLocalDirectory')}
              </ariaComponents.Button>
            </div>
          )}
        </div>
      </AnimatedBackground>
    </div>
  )
}

export default React.memo(CategorySwitcher)
