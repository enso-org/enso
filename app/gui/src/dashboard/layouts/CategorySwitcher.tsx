/** @file Switcher to choose the currently visible assets table category. */
import * as aria from '#/components/aria'
import { Badge } from '#/components/Badge'
import { Button, BUTTON_STYLES } from '#/components/Button'
import { Dialog } from '#/components/Dialog'
import { Text } from '#/components/Text'
import * as mimeTypes from '#/data/mimeTypes'
import { useAriaDragDelayAction } from '#/hooks/dragDelayHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import * as offlineHooks from '#/hooks/offlineHooks'
import {
  areCategoriesEqual,
  ASSETS_DATA_TRANSFER_PAYLOAD,
  canTransferBetweenCategories,
  useTransferBetweenCategories,
  type Category,
} from '#/layouts/Drive/Categories'
import { useCategoriesAPI } from '#/layouts/Drive/Categories/categoriesHooks'
import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import { setDriveLocation } from '#/providers/DriveProvider'
import { setModal, unsetModal } from '#/providers/ModalProvider'
import { tv } from '#/utilities/tailwindVariants'
import { SEARCH_PARAMS_PREFIX } from '$/appUtils'
import * as authProvider from '$/providers/react'
import { useBackends, useRouter, useText } from '$/providers/react'
import { Path } from 'enso-common/src/services/Backend'
import * as React from 'react'
import { twJoin } from 'tailwind-merge'

/** Metadata for a category. */
interface CategoryMetadata {
  readonly isNested?: boolean
  readonly category: Category
  readonly buttonLabel: string
  readonly dropZoneLabel: string
  readonly className?: string
  readonly iconClassName?: string
}

/** Props for a {@link CategorySwitcherItem}. */
interface InternalCategorySwitcherItemProps extends CategoryMetadata {
  readonly badgeContent?: React.ReactNode
  readonly isDisabled: boolean
  readonly disabledReason?: string | null | undefined
  readonly onPress?: (() => void) | undefined
}

const CATEGORY_SWITCHER_VARIANTS = tv({
  extend: BUTTON_STYLES,
  base: 'group opacity-90 transition-opacity group-hover:bg-white/100 w-auto max-w-full',
  slots: {
    wrapper: 'w-full',
    text: 'flex-1 min-w-0 w-auto items-start justify-start',
  },
})

/** An entry in a {@link CategorySwitcher}. */
function CategorySwitcherItem(props: InternalCategorySwitcherItemProps) {
  const {
    badgeContent,
    isDisabled = false,
    disabledReason,
    isNested = false,
    category,
    buttonLabel,
    dropZoneLabel,
  } = props
  const { icon, label } = category
  const { category: currentCategory } = useCategoriesAPI()

  const [isTransitioning, startTransition] = React.useTransition()

  const { getText } = useText()

  const isCurrent = areCategoriesEqual(currentCategory, category)

  const transferBetweenCategories = useTransferBetweenCategories(currentCategory)

  const tooltip = (isDisabled && disabledReason) ?? false

  const isDropTarget =
    !areCategoriesEqual(currentCategory, category) &&
    canTransferBetweenCategories(currentCategory, category)
  const acceptedDragTypes = isDropTarget ? [mimeTypes.ASSETS_MIME_TYPE] : []

  const onPress = useEventCallback(() => {
    if (!isDisabled) {
      // We use startTransition to trigger a background transition between categories.
      // and to not invoke the Suspense boundary.
      // This makes the transition feel more responsive and natural.
      startTransition(() => {
        props.onPress?.()
        setDriveLocation(null, category.id)
      })
    }
  })

  const onDrop = useEventCallback(async (event: aria.DropEvent) => {
    unsetModal()
    if (event.dropOperation === 'cancel') return
    const payloads = await Promise.all(
      event.items
        .filter((item) => item.kind === 'text')
        .map(async (item) => {
          const text = await item.getText(mimeTypes.ASSETS_MIME_TYPE)
          const parsedPayload = ASSETS_DATA_TRANSFER_PAYLOAD.safeParse(JSON.parse(text))
          return parsedPayload.success ? parsedPayload.data : null
        }),
    ).then((items) => items.filter((payload) => payload != null))
    const firstItem = payloads[0]?.items[0]

    const transfer = async () => {
      await Promise.all(
        payloads.map((payload) =>
          transferBetweenCategories(
            payload.category,
            category,
            payload.items,
            null,
            event.dropOperation,
          ),
        ),
      )
    }

    if (category.type === 'trash') {
      setModal(
        <ConfirmDeleteModal
          defaultOpen
          actionText={
            payloads[0]?.items.length === 1 && firstItem != null ?
              getText('deleteSelectedAssetActionText', firstItem.title)
            : getText(
                'deleteSelectedAssetsActionText',
                payloads.flatMap(({ items }) => items).length,
              )
          }
          onConfirm={transfer}
        />,
      )
    } else {
      await transfer()
    }
  })

  const dragDelayProps = useAriaDragDelayAction(onPress)

  const element = (
    <aria.DropZone
      aria-label={dropZoneLabel}
      getDropOperation={(types) => {
        if (acceptedDragTypes.some((type) => types.has(type))) return 'move'
        return 'cancel'
      }}
      className="group relative flex w-full min-w-0 flex-auto items-start rounded-full drop-target-after"
      {...aria.mergeProps<aria.DropZoneProps>()({ onDrop }, dragDelayProps)}
    >
      <div className={twJoin('w-auto max-w-full', isCurrent && 'rounded-full bg-invert')}>
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
      </div>
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
  readonly onChange?: (() => void) | undefined
}

/** A switcher to choose the currently visible assets table categoryModule.categoryType. */
function CategorySwitcher(props: CategorySwitcherProps) {
  const { onChange } = props

  const { router } = useRouter()
  const { getText } = useText()

  const { isOffline } = offlineHooks.useOffline()
  const { localBackend } = useBackends()
  const { user } = authProvider.useFullUserSession()

  const { cloudCategories, localCategories } = useCategoriesAPI()

  const { cloudCategory, recentCategory, trashCategory, teamCategories } = cloudCategories
  const { localCategory, directories, addDirectory, removeDirectory } = localCategories

  const cloudDisabledReason = React.useMemo(() => {
    if (isOffline) {
      return getText('unavailableOffline')
    } else if (!user.isEnabled) {
      return getText('notEnabledSubtitle')
    } else {
      return null
    }
  }, [isOffline, user, getText])
  const isCloudDisabled = cloudDisabledReason != null

  const localDisabledReason = React.useMemo(() => {
    if (localBackend == null) {
      return getText('localBackendNotDetectedError')
    } else {
      return null
    }
  }, [localBackend, getText])
  const isLocalDisabled = localDisabledReason != null

  return (
    <div
      aria-label={getText('categorySwitcherMenuLabel')}
      role="grid"
      className="flex flex-col items-start text-primary"
    >
      <CategorySwitcherItem
        category={cloudCategory}
        isDisabled={isCloudDisabled}
        disabledReason={cloudDisabledReason}
        buttonLabel={getText('cloudCategoryButtonLabel')}
        dropZoneLabel={getText('cloudCategoryDropZoneLabel')}
        badgeContent={getText('cloudCategoryBadgeContent')}
        onPress={onChange}
      />
      {teamCategories.map((teamCategory) => (
        <CategorySwitcherItem
          key={teamCategory.id}
          isNested
          category={teamCategory}
          isDisabled={isCloudDisabled}
          disabledReason={cloudDisabledReason}
          buttonLabel={getText('teamCategoryButtonLabel', teamCategory.team.name)}
          dropZoneLabel={getText('teamCategoryDropZoneLabel', teamCategory.team.name)}
          onPress={onChange}
        />
      ))}
      <CategorySwitcherItem
        isNested
        category={recentCategory}
        isDisabled={isCloudDisabled}
        disabledReason={cloudDisabledReason}
        buttonLabel={getText('recentCategoryButtonLabel')}
        dropZoneLabel={getText('recentCategoryDropZoneLabel')}
        onPress={onChange}
      />
      <CategorySwitcherItem
        isNested
        category={trashCategory}
        isDisabled={isCloudDisabled}
        disabledReason={cloudDisabledReason}
        buttonLabel={getText('trashCategoryButtonLabel')}
        dropZoneLabel={getText('trashCategoryDropZoneLabel')}
        onPress={onChange}
      />
      {localCategory != null && (
        <div className="group flex items-center gap-2 self-stretch drop-target-after">
          <CategorySwitcherItem
            category={localCategory}
            isDisabled={isLocalDisabled}
            disabledReason={localDisabledReason}
            buttonLabel={getText('localCategoryButtonLabel')}
            dropZoneLabel={getText('localCategoryDropZoneLabel')}
            onPress={onChange}
          />

          <Button
            size="medium"
            variant="icon"
            extraClickZone="small"
            icon="settings"
            aria-label={getText('changeLocalRootDirectoryInSettings')}
            className="my-auto opacity-0 transition-opacity group-hover:opacity-100"
            onPress={() => {
              void router.push({
                path: '/settings',
                query: { [`${SEARCH_PARAMS_PREFIX}SettingsTab`]: JSON.stringify('local') },
              })
            }}
          />
        </div>
      )}
      {directories != null &&
        directories.map((directory) => (
          <div key={directory.id} className="group flex items-center gap-2 self-stretch">
            <CategorySwitcherItem
              isNested
              category={directory}
              isDisabled={isLocalDisabled}
              disabledReason={localDisabledReason}
              buttonLabel={getText('localCategoryButtonLabel')}
              dropZoneLabel={getText('localCategoryDropZoneLabel')}
              onPress={onChange}
            />

            <Dialog.Trigger>
              <Button
                size="medium"
                variant="icon"
                extraClickZone={false}
                icon="minus"
                aria-label={getText('removeDirectoryFromFavorites')}
                showIconOnHover
              />

              <ConfirmDeleteModal
                actionText={getText('removeTheLocalDirectoryXFromFavorites', directory.label)}
                actionButtonLabel={getText('remove')}
                onConfirm={() => {
                  removeDirectory(directory.id)
                }}
              />
            </Dialog.Trigger>
          </div>
        ))}

      {directories != null && window.api && (
        <div className="flex">
          <div className="ml-[15px] mr-1.5 rounded-full border-r border-primary/20" />

          <Button
            size="medium"
            variant="icon"
            icon="folder_add_small"
            loaderPosition="icon"
            onPress={async () => {
              const [newDirectory] =
                (await window.api?.fileBrowser.openFileBrowser('directory')) ?? []

              if (newDirectory != null) {
                const addedDirectory = directories.find(
                  (directory) => directory.rootPath === newDirectory,
                )
                const newCategory = addedDirectory ?? addDirectory(Path(newDirectory))
                setDriveLocation(null, newCategory.id)
              }
            }}
          >
            {getText('addLocalDirectory')}
          </Button>
        </div>
      )}
    </div>
  )
}

export default React.memo(CategorySwitcher)
