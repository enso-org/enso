/** @file A modal to select labels for an asset. */
import { Button } from '#/components/Button'
import { Check } from '#/components/Check'
import ColorPicker from '#/components/ColorPicker'
import { Dialog, Popover } from '#/components/Dialog'
import { Form } from '#/components/Form'
import { Input } from '#/components/Inputs/Input'
import { Scroller } from '#/components/Scroller'
import { Separator } from '#/components/Separator'
import { Text } from '#/components/Text'
import { backendMutationOptions, backendQueryOptions } from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import type { SelectedAssetInfo } from '#/providers/DriveProvider'
import { tv } from '#/utilities/tailwindVariants'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useText } from '$/providers/react'
import { useSuspenseQuery } from '@tanstack/react-query'
import type { Backend } from 'enso-common/src/services/Backend'
import {
  COLORS,
  colorsAreEqual,
  findLeastUsedColor,
  LabelName,
  lChColorToCssColor,
  type Label,
  type LChColor,
} from 'enso-common/src/services/Backend'
import { useMemo } from 'react'
import { Tag, TagGroup, TagList, useFilter } from 'react-aria-components'
import type { ControllerRenderProps } from 'react-hook-form'
import ConfirmDeleteModal from './ConfirmDeleteModal'

/** Props for a {@link ManageLabelsModal}. */
export interface ManageLabelsModalProps {
  readonly backend: Backend
  readonly items: readonly SelectedAssetInfo[]
  readonly triggerRef?: React.MutableRefObject<HTMLElement | null>
}

/**
 * A modal to select labels for an asset.
 * @throws {Error} when the current backend is the local backend, or when the user is offline.
 * This should never happen, as this modal should not be accessible in either case.
 */
export default function ManageLabelsModal(props: ManageLabelsModalProps) {
  const { triggerRef } = props

  return (
    <Popover
      size="custom"
      className="max-w-64 overflow-y-hidden"
      {...(triggerRef ? { triggerRef } : {})}
      shouldCloseOnInteractOutside={() => true}
    >
      <ManageLabelsForm {...props} />
    </Popover>
  )
}

const MANAGE_LABELS_MODAL_STYLES = tv({
  base: 'flex flex-wrap gap-4 max-w-full flex-1 basis-0 min-h-0',
  slots: {
    allLabels: 'flex flex-col w-full px-1.5 pr-0 min-h-0',
    labels: 'flex flex-col',
    label:
      'flex flex-none items-center justify-center max-w-full min-w-10 px-1.5 py-0 rounded-3xl bg-primary',
    itemLabels: 'inline-flex flex-none max-w-full w-full flex-wrap gap-0.5 px-2',
    itemLabelsList: 'inline-flex max-w-full flex-wrap gap-0.5',
    input: 'w-full flex-1 px-1 py-1',
  },
})

/** Metadata for a label. */
interface LabelInfo {
  readonly label: Label
  readonly state: 'all' | 'none' | 'some'
}

/** Form for {@link ManageLabelsModal}. */
function ManageLabelsForm(props: ManageLabelsModalProps) {
  const { backend, items } = props

  const { getText } = useText()
  const { data: allLabels } = useSuspenseQuery(backendQueryOptions(backend, 'listTags', []))
  const leastUsedColor = findLeastUsedColor(allLabels)

  const labelsPresence = useMemo(
    () =>
      allLabels.map<LabelInfo>((label) => {
        const count = items.filter((item) => item.labels?.includes(label.value) === true).length
        return {
          label,
          state:
            count === items.length ? ('all' as const)
            : count === 0 ? ('none' as const)
            : ('some' as const),
        }
      }),
    [allLabels, items],
  )

  const styles = MANAGE_LABELS_MODAL_STYLES()

  const createTag = useMutationCallback(backendMutationOptions(backend, 'createTag'))
  const associateTag = useMutationCallback(backendMutationOptions(backend, 'associateTag'))
  const deleteTag = useMutationCallback(backendMutationOptions(backend, 'deleteTag'))

  const createLabel = useEventCallback(async (name: string, color?: LChColor) => {
    const labelName = LabelName(name)
    const newLabel = await createTag([{ value: labelName, color: color ?? leastUsedColor }])
    await Promise.allSettled(
      items.map((item) => associateTag([item.id, [...(item.labels ?? []), labelName], item.title])),
    )
    form.setValue('labels', [...form.getValues('labels'), { label: newLabel, state: 'all' }])
  })

  const deleteLabel = useEventCallback((label: Label) => {
    form.setValue(
      'labels',
      selectedLabels.filter((l) => l.label.id !== label.id),
    )
    return deleteTag([label.id, label.value])
  })

  const form = Form.useForm({
    schema: (z) =>
      z.object({
        labels: z
          .object({ label: z.custom<Label>(), state: z.enum(['none', 'some', 'all']) })
          .array()
          .readonly(),
        query: z.string(),
      }),
    defaultValues: { labels: labelsPresence, query: '' },
  })

  const onChange = useEventCallback(
    async (
      field: ControllerRenderProps<
        {
          labels: readonly { label: Label; state: 'all' | 'none' | 'some' }[]
          query: string
        },
        'labels'
      >,
      newSelectedLabels: readonly LabelInfo[],
    ) => {
      const previousLabels = field.value
      field.onChange(newSelectedLabels)
      const deltas = previousLabels.flatMap((previousLabel) => {
        const newLabel = newSelectedLabels.find(
          (otherLabel) => otherLabel.label.id === previousLabel.label.id,
        )
        if (!newLabel || newLabel.state === previousLabel.state) {
          return []
        }
        return { previous: previousLabel, current: newLabel }
      })
      return await Promise.allSettled(
        items.map((item) => {
          let isChanged = false
          for (const { previous, current } of deltas) {
            const wasLabelPresent = (() => {
              switch (previous.state) {
                case 'all':
                  return true
                case 'none':
                  return false
                case 'some':
                  return item.labels?.includes(previous.label.value) === true
              }
            })()
            switch (current.state) {
              case 'all': {
                if (!wasLabelPresent) {
                  isChanged = true
                }
                break
              }
              case 'none': {
                if (wasLabelPresent) {
                  isChanged = true
                }
                break
              }
              case 'some': {
                break
              }
            }
            if (isChanged) {
              break
            }
          }
          if (!isChanged) {
            return Promise.resolve()
          }
          const newLabels = new Set(item.labels ?? [])
          for (const label of newSelectedLabels) {
            switch (label.state) {
              case 'all': {
                newLabels.add(label.label.value)
                break
              }
              case 'none': {
                newLabels.delete(label.label.value)
                break
              }
              case 'some': {
                break
              }
            }
          }
          return associateTag([item.id, [...newLabels], item.title])
        }),
      )
    },
  )

  const selectedLabels = Form.useWatch({ control: form.control, name: 'labels' })

  return (
    <Form form={form} gap="none" className="flex max-h-[inherit] w-full flex-col">
      <Input
        name="query"
        type="search"
        variant="custom"
        size="small"
        fieldClassName={styles.input()}
        placeholder={getText('search.placeholder')}
        autoFocus
      />

      {selectedLabels.length > 0 && (
        <div className={styles.itemLabels()}>
          <Scroller background="secondary">
            <TagGroup className="contents" aria-label={getText('manageLabelsModal.selectedLabels')}>
              <TagList
                className="flex w-full gap-1"
                items={selectedLabels
                  .map((label) =>
                    allLabels.find((allLabelsItem) => allLabelsItem.id === label.label.id),
                  )
                  .filter((label) => label !== undefined)}
              >
                {(label) => (
                  <Tag
                    key={label.id}
                    id={label.id}
                    textValue={label.value}
                    style={{ backgroundColor: lChColorToCssColor(label.color) }}
                    className={styles.label()}
                  >
                    <Text truncate color="invert" textSelection="none">
                      {label.value}
                    </Text>
                  </Tag>
                )}
              </TagList>
            </TagGroup>
          </Scroller>
        </div>
      )}

      <Separator className="my-2" />

      <div className={styles.allLabels()}>
        <Text variant="body" color="muted" weight="semibold" className="ml-2">
          {getText('manageLabelsModal.allLabels')}
        </Text>

        <Form.FieldValue form={form} name="query">
          {(query) => (
            <Form.Controller
              control={form.control}
              name="labels"
              render={({ field }) => (
                <AllLabels
                  colors={COLORS}
                  leastUsedColor={leastUsedColor}
                  labels={field.value}
                  query={query}
                  onCreateLabel={createLabel}
                  onDeleteLabel={deleteLabel}
                  onCheckLabel={(newLabel) =>
                    onChange(
                      field,
                      selectedLabels.map((l) =>
                        l.label.id === newLabel.id ? { ...l, state: 'all' } : l,
                      ),
                    )
                  }
                  onUncheckLabel={(label) =>
                    onChange(
                      field,
                      selectedLabels.map((l) =>
                        l.label.id === label.id ? { ...l, state: 'none' } : l,
                      ),
                    )
                  }
                />
              )}
            />
          )}
        </Form.FieldValue>
      </div>

      <div className="flex w-full flex-col gap-2 px-2 py-2">
        <Button.Group width="full" align="between" gap="small">
          <Popover.Trigger>
            <Button variant="icon" size="small" fullWidth icon="add">
              {getText('manageLabelsModal.createLabel')}
            </Button>

            <Popover>
              <Form
                schema={(z) =>
                  z.object({
                    name: z
                      .string()
                      .trim()
                      .min(1)
                      .refine((value) => !allLabels.some((label) => label.value === value), {
                        message: getText('manageLabelsModal.labelAlreadyExists'),
                      }),
                    color: z.custom<LChColor>(),
                  })
                }
                defaultValues={{ name: '', color: leastUsedColor }}
                method="dialog"
                onSubmit={({ name, color }) => createLabel(name, color)}
              >
                <Input name="name" label={getText('name')} autoFocus />

                <Form.Controller
                  name="color"
                  render={({ field }) => (
                    <Form.Field name="color" label={getText('manageLabelsModal.chooseColor')}>
                      <ColorPicker
                        aria-label={getText('manageLabelsModal.chooseColor')}
                        name="color"
                        defaultValue={lChColorToCssColor(leastUsedColor)}
                        // eslint-disable-next-line @typescript-eslint/no-unsafe-argument
                        value={lChColorToCssColor(field.value)}
                        setColor={(color) => {
                          field.onChange(color)
                        }}
                      />
                    </Form.Field>
                  )}
                />

                <Form.Submit className="ml-auto min-w-12" size="small">
                  {getText('manageLabelsModal.createLabel')}
                </Form.Submit>

                <Form.FormError />
              </Form>
            </Popover>
          </Popover.Trigger>
        </Button.Group>

        <Form.FormError />
      </div>
    </Form>
  )
}

/** Props for a {@link AllLabels}. */
interface AllLabelsProps {
  readonly colors: readonly LChColor[]
  readonly leastUsedColor: LChColor
  readonly labels: readonly LabelInfo[]
  readonly query: string
  readonly onCheckLabel: (label: Label) => void
  readonly onUncheckLabel: (label: Label) => void
  readonly onCreateLabel: (name: string, color: LChColor) => Promise<void>
  readonly onDeleteLabel: (label: Label) => Promise<void>
}

/** A list of all labels. */
function AllLabels(props: AllLabelsProps) {
  const {
    labels,
    query,
    colors,
    leastUsedColor,
    onCreateLabel,
    onDeleteLabel,
    onCheckLabel,
    onUncheckLabel,
  } = props

  const { getText } = useText()
  const filter = useFilter({ sensitivity: 'base' })
  const filteredLabels = labels.filter((label) => filter.contains(label.label.value, query))

  return (
    <div
      aria-label={getText('manageLabelsModal.allLabels')}
      className="flex max-h-72 flex-col overflow-y-auto overflow-x-hidden scroll-offset-edge-0"
    >
      {filteredLabels.length === 0 && (
        <NotFoundLabel
          query={query}
          onCreateLabel={onCreateLabel}
          leastUsedColor={leastUsedColor}
          colors={colors}
        />
      )}
      {filteredLabels.map(({ label, state }) => (
        <div key={label.id} id={label.id} className="group rounded-3xl pressed:bg-primary/5">
          <div className="flex w-full items-center gap-2 px-2 py-0.5 hover:rounded-3xl hover:bg-primary/5">
            <Button
              variant="custom"
              onPress={() => {
                if (state === 'all') {
                  onUncheckLabel(label)
                } else {
                  onCheckLabel(label)
                }
              }}
            >
              <Check isSelected={state === 'all'} isIndeterminate={state === 'some'} />

              <div
                className="aspect-square w-4 flex-none rounded-full"
                style={{ backgroundColor: lChColorToCssColor(label.color) }}
              />

              <Text truncate nowrap textSelection="none">
                {label.value}
              </Text>
            </Button>

            <Dialog.Trigger>
              <Button
                variant="icon"
                aria-label={getText('delete')}
                icon="trash_small"
                size="small"
                className="ml-auto opacity-0 transition-opacity duration-75 group-hover:opacity-100"
              />

              <ConfirmDeleteModal
                cannotUndo
                actionText={getText('deleteLabelActionText', label.value)}
                actionButtonLabel={getText('delete')}
                onConfirm={() => onDeleteLabel(label)}
              />
            </Dialog.Trigger>
          </div>
        </div>
      ))}
    </div>
  )
}

/**
 * Props for a {@link ColorSwitcher}.
 */
interface ColorSwitcherProps {
  readonly name: string
  readonly color: LChColor
  readonly colors: readonly LChColor[]
  readonly leastUsedColor: LChColor
}

/**
 * A color picker for a label.
 */
function ColorSwitcher(props: ColorSwitcherProps) {
  const { name, color, colors, leastUsedColor } = props

  const { getText } = useText()

  const { formInstance } = Form.useFieldRegister({
    name,
  })

  const rotateColor = useEventCallback(() => {
    const index = colors.findIndex((item) => colorsAreEqual(item, color))
    const nextColor = colors[(index + 1) % colors.length]

    formInstance.setValue(name, nextColor ?? leastUsedColor)
  })

  return (
    <Button
      variant="icon"
      size="custom"
      className="aspect-square h-4 w-4"
      tooltip={getText('manageLabelsModal.nextColor')}
      aria-label={getText('manageLabelsModal.nextColor')}
      onPress={rotateColor}
      style={{ backgroundColor: lChColorToCssColor(color) }}
    />
  )
}

/**
 * Props for a {@link NotFoundLabel}.
 */
interface NotFoundLabelProps {
  readonly query: string
  readonly onCreateLabel: (name: string, color: LChColor) => Promise<void>
  readonly leastUsedColor: LChColor
  readonly colors: readonly LChColor[]
}

/**
 * A component that displays a label that does not exist.
 * Offers a form to create a new label.
 */
function NotFoundLabel(props: NotFoundLabelProps) {
  const { query, onCreateLabel, leastUsedColor, colors } = props

  const { getText } = useText()

  const form = Form.useForm({
    schema: (z) => z.object({ color: z.custom<LChColor>() }),
    defaultValues: { color: leastUsedColor },
    onSubmit: ({ color }) => onCreateLabel(query, color),
  })

  return (
    <Button.Group verticalAlign="center" gap="xxsmall" align="center" className="my-4">
      <Form.FieldValue form={form} name="color">
        {(color) => (
          <ColorSwitcher
            name="color"
            color={color}
            colors={colors}
            leastUsedColor={leastUsedColor}
          />
        )}
      </Form.FieldValue>

      <Form.Submit form={form} variant="icon" size="small" onPress={() => form.submit()}>
        {getText('manageLabelsModal.createLabelWithTitle', query)}
      </Form.Submit>
    </Button.Group>
  )
}
