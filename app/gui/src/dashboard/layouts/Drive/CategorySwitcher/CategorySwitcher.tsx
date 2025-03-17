/** @file Switcher to choose the currently visible assets table category. */
import { SEARCH_PARAMS_PREFIX } from '#/appUtils'
import FolderAddIcon from '#/assets/folder_add.svg'
import Minus2Icon from '#/assets/minus2.svg'
import SettingsIcon from '#/assets/settings.svg'
import { AnimatedBackground } from '#/components/AnimatedBackground'
import { DropZone, type DropEvent } from '#/components/aria'
import { BUTTON_STYLES, Button, DialogTrigger, Text } from '#/components/AriaComponents'
import { Badge } from '#/components/Badge'
import { ASSETS_MIME_TYPE } from '#/data/mimeTypes'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useOffline } from '#/hooks/offlineHooks'
import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useLocalBackend } from '#/providers/BackendProvider'
import { useSetCurrentDirectoryId } from '#/providers/DriveProvider'
import { useSetModal } from '#/providers/ModalProvider'
import { useText } from '#/providers/TextProvider'
import type { AssetId } from '#/services/Backend'
import { tv } from '#/utilities/tailwindVariants'
import { memo, useTransition, type ReactNode } from 'react'
import { useSearchParams } from 'react-router-dom'
import { twJoin } from 'tailwind-merge'
import { areCategoriesEqual, canTransferBetweenCategories, type Category } from './Category'
import { useCloudCategoryList, useLocalCategoryList, useTransferBetweenCategories } from './hooks'

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
  readonly badgeContent?: ReactNode
  readonly isDisabled: boolean
}

const CATEGORY_SWITCHER_VARIANTS = tv({
  extend: BUTTON_STYLES,
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

  const [isTransitioning, startTransition] = useTransition()

  const { user } = useFullUserSession()
  const { unsetModal } = useSetModal()
  const { getText } = useText()
  const localBackend = useLocalBackend()
  const { isOffline } = useOffline()
  const setCurrentDirectoryId = useSetCurrentDirectoryId()

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
  const acceptedDragTypes = isDropTarget ? [ASSETS_MIME_TYPE] : []

  const onPress = useEventCallback(() => {
    if (error == null) {
      // We use startTransition to trigger a background transition between categories.
      // and to not invoke the Suspense boundary.
      // This makes the transition feel more responsive and natural.
      startTransition(() => {
        setCurrentDirectoryId({
          current: null,
          parent: null,
        })
        setCategoryId(category.id)
      })
    }
  })

  const onDrop = useEventCallback((event: DropEvent) => {
    unsetModal()
    void Promise.all(
      event.items.flatMap(async (item) => {
        if (item.kind === 'text') {
          const text = await item.getText(ASSETS_MIME_TYPE)
          const payload: unknown = JSON.parse(text)
          return Array.isArray(payload) ?
              payload.flatMap((key) =>
                // This is SAFE, assuming only this app creates payloads with
                // the specific mimetype above.
                // eslint-disable-next-line no-restricted-syntax
                typeof key === 'string' ? [key as AssetId] : [],
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
    <DropZone
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
        <Button
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
          <Text disableLineHeightCompensation weight="semibold" color="current" truncate="1">
            {label}
          </Text>
        </Button>
      </AnimatedBackground.Item>
      <div className="absolute left-full ml-2 hidden group-focus-visible:block">
        {getText('drop')}
      </div>
    </DropZone>
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
export const CategorySwitcher = memo(function CategorySwitcher(props: CategorySwitcherProps) {
  const { category, setCategoryId } = props

  const { getText } = useText()
  const [, setSearchParams] = useSearchParams()

  const { isOffline } = useOffline()

  const cloudCategories = useCloudCategoryList()
  const localCategories = useLocalCategoryList()

  const itemProps = { currentCategory: category, setCategoryId }

  const { cloudCategory, recentCategory, trashCategory, teamCategories } = cloudCategories
  const { localCategory, directories, addDirectory, removeDirectory } = localCategories

  return (
    <div className="flex flex-col gap-2">
      <AnimatedBackground>
        <Text variant="subtitle" weight="semibold">
          {getText('category')}
        </Text>

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

              <Button
                size="medium"
                variant="icon"
                extraClickZone="small"
                icon={SettingsIcon}
                aria-label={getText('changeLocalRootDirectoryInSettings')}
                className="my-auto opacity-0 transition-opacity group-hover:opacity-100"
                onPress={() => {
                  setSearchParams({
                    [`${SEARCH_PARAMS_PREFIX}SettingsTab`]: JSON.stringify('local'),
                    [`${SEARCH_PARAMS_PREFIX}page`]: JSON.stringify('settings'),
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

                <DialogTrigger>
                  <Button
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
                </DialogTrigger>
              </div>
            ))}

          {directories != null && window.fileBrowserApi && (
            <div className="flex">
              <div className="ml-[15px] mr-1.5 rounded-full border-r border-primary/20" />

              <Button
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
              </Button>
            </div>
          )}
        </div>
      </AnimatedBackground>
    </div>
  )
})
