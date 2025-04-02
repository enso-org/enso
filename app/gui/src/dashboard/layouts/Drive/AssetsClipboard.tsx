/** @file A clipboard for assets. */
import type { DropEvent } from '#/components/aria'
import { DropZone } from '#/components/aria'
import { Form, Selector, Text, Underlay } from '#/components/AriaComponents'
import { Badge } from '#/components/Badge'
import { INITIAL_ROW_STATE } from '#/components/dashboard/AssetRow/assetRowUtils'
import AssetNameColumn from '#/components/dashboard/column/NameColumn'
import { ASSETS_MIME_TYPE } from '#/data/mimeTypes'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useSyncRef } from '#/hooks/syncRefHooks'
import type { AssetsTableState } from '#/layouts/AssetsTable'
import { canTransferBetweenCategories, type Category } from '#/layouts/CategorySwitcher/Category'
import { useGetAsset } from '#/layouts/Drive/assetsTableItemsHooks'
import DragModal from '#/modals/DragModal'
import { useBackend } from '#/providers/BackendProvider'
import { usePasteData, useSetPasteData } from '#/providers/DriveProvider'
import { setModal, unsetModal } from '#/providers/ModalProvider'
import { useText } from '#/providers/TextProvider'
import type { AssetId } from '#/services/Backend'
import type { AssetRowsDragPayload } from '#/utilities/drag'
import { ASSET_ROWS, setDragImageToBlank } from '#/utilities/drag'
import type { PasteType } from '#/utilities/pasteData'
import { noop } from '@vueuse/core'
import { useEffect, type DragEvent } from 'react'
import { twJoin } from 'tailwind-merge'

/** Props for a {@link AssetsClipboard}. */
export interface AssetClipboardProps {
  readonly category: Category
}

/** A clipboard for assets. */
export function AssetsClipboard(props: AssetClipboardProps) {
  const { category } = props

  const { getText } = useText()

  const form = Form.useForm({
    schema: (z) =>
      z.object({
        pasteType: z.enum(['copy', 'move']),
      }),
    defaultValues: { pasteType: 'copy' },
  })
  const formRef = useSyncRef(form)

  const pasteData = usePasteData()
  const setPasteData = useSetPasteData()
  const pasteDataRef = useSyncRef(pasteData)
  const pasteType = Form.useWatch({ control: form.control, name: 'pasteType' })
  const pasteTypeRef = useSyncRef(pasteType)

  useEffect(() => {
    if (pasteData && pasteTypeRef.current !== pasteData.type) {
      formRef.current.resetField('pasteType', { defaultValue: pasteData.type })
    }
  }, [formRef, pasteData, pasteTypeRef])

  useEffect(() => {
    if (pasteDataRef.current && pasteType !== pasteDataRef.current.type) {
      setPasteData({ ...pasteDataRef.current, type: pasteType })
    }
  }, [pasteType, pasteDataRef, setPasteData])

  return (
    <div className="flex flex-none flex-col">
      <Form form={form} data-testid="clipboard" className="flex flex-col items-start gap-2">
        <Text variant="subtitle" elementType="h3" weight="semibold">
          {getText('clipboard')}
        </Text>

        <Selector
          form={form}
          aria-label={getText('pasteTypeSelector')}
          name="pasteType"
          size="small"
          variant="ghost"
          items={['copy', 'move'] as const}
        >
          {(otherPasteType) => getText(otherPasteType)}
        </Selector>

        <AssetsClipboardItems pasteType={pasteType} category={category} />
      </Form>
    </div>
  )
}

/** Props for a {@link AssetsClipboardItems}. */
interface AssetsClipboardItemsProps {
  readonly pasteType: PasteType
  readonly category: Category
}

/** A placeholder for when the clipboard is empty. */
function AssetsClipboardItems(props: AssetsClipboardItemsProps) {
  const { pasteType, category } = props

  const backend = useBackend(category)
  const { getText } = useText()
  const getAsset = useGetAsset()

  const pasteData = usePasteData()
  const setPasteData = useSetPasteData()
  const effectivePasteData =
    (
      pasteData?.data.backendType === backend.type &&
      canTransferBetweenCategories(pasteData.data.category, category)
    ) ?
      pasteData
    : null
  const pasteAssets = [...(pasteData?.data.ids ?? [])].map(getAsset).filter((x) => x != null)

  const itemCount = effectivePasteData?.data.ids.size ?? 0
  const clipboardContentsDescription = getText(
    pasteType === 'copy' ? 'xItemsCopied' : 'xItemsCut',
    itemCount,
  )

  const partialState =
    /// UNSAFE type assertion.
    // eslint-disable-next-line no-restricted-syntax
    { backend, category } satisfies Partial<AssetsTableState> as unknown as AssetsTableState

  const onDrop = useEventCallback((event: DropEvent) => {
    unsetModal()
    void Promise.all(
      event.items.flatMap(async (item) => {
        if (item.kind === 'text') {
          const text = await item.getText(ASSETS_MIME_TYPE)
          const payload: unknown = JSON.parse(text)
          return Array.isArray(payload) ?
              payload.flatMap((id) =>
                // This is SAFE, assuming only this app creates payloads with
                // the specific mimetype above.
                // eslint-disable-next-line no-restricted-syntax
                typeof id === 'string' ? [id as AssetId] : [],
              )
            : []
        } else {
          return []
        }
      }),
    ).then((ids) => {
      setPasteData({
        type: pasteType,
        data: { backendType: backend.type, category, ids: new Set(ids.flat(1)) },
      })
    })
  })

  return (
    <DropZone
      aria-label={getText('clipboardDropZone')}
      getDropOperation={(types) => (types.has(ASSETS_MIME_TYPE) ? 'move' : 'cancel')}
      className={twJoin(
        'group flex min-h-20 w-full rounded-2xl border-dashed border-primary/20',
        !pasteData && 'items-center border-2',
      )}
      onDrop={onDrop}
    >
      {!pasteData && (
        <Text className="cursor-copy text-center text-primary/40 transition-colors group-hover:text-primary">
          {getText('dropItemsHereToSetClipboard')}
        </Text>
      )}
      {pasteData && (
        <>
          <div
            aria-label={clipboardContentsDescription}
            draggable={effectivePasteData != null}
            className="relative flex w-full flex-col items-center"
            onDragStart={(event: DragEvent<HTMLDivElement>) => {
              const nodes = pasteAssets
              const payload: AssetRowsDragPayload = nodes.map((node) => ({
                key: node.id,
                asset: node,
              }))
              event.dataTransfer.setData(
                ASSETS_MIME_TYPE,
                JSON.stringify(nodes.map((node) => node.id)),
              )
              setDragImageToBlank(event)
              ASSET_ROWS.bind(event, payload)
              setModal(
                <DragModal
                  event={event}
                  className="flex flex-col rounded-default bg-selected-frame backdrop-blur-default"
                  onDragEnd={() => {
                    ASSET_ROWS.unbind(payload)
                  }}
                >
                  {nodes.map((node) => (
                    <AssetNameColumn
                      isNavigating={false}
                      key={node.id}
                      item={node}
                      isOpened={false}
                      backendType={backend.type}
                      state={partialState}
                      rowState={INITIAL_ROW_STATE}
                      // The drag placeholder cannot be interacted with.
                      isPlaceholder={false}
                      setSelected={noop}
                      setRowState={noop}
                      isEditable={false}
                      labels={[]}
                    />
                  ))}
                </DragModal>,
              )
            }}
          >
            <Text className={effectivePasteData == null ? 'rounded-full bg-invert px-2' : ''}>
              {effectivePasteData == null ? getText('cannotPasteHere') : '\u200a'}
            </Text>
            {pasteAssets.slice(0, 3).map((node) => (
              <AssetNameColumn
                isNavigating={false}
                key={node.id}
                item={node}
                isOpened={false}
                backendType={backend.type}
                state={partialState}
                rowState={INITIAL_ROW_STATE}
                // The drag placeholder cannot be interacted with.
                isPlaceholder={false}
                setSelected={noop}
                setRowState={noop}
                isEditable={false}
                labels={[]}
              />
            ))}

            <Underlay className="absolute -right-1 top-2 rounded-full">
              <Badge color="primary">{pasteData.data.ids.size}</Badge>
            </Underlay>
          </div>
        </>
      )}
    </DropZone>
  )
}
