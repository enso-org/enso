/** @file A modal to select labels for an asset. */
import { AnimatedBackground } from '#/components/AnimatedBackground'
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
import type Backend from '#/services/Backend'
import {
  COLORS,
  colorsAreEqual,
  findLeastUsedColor,
  LabelName,
  lChColorToCssColor,
  type AnyAsset,
  type Label,
  type LChColor,
} from '#/services/Backend'
import { tv } from '#/utilities/tailwindVariants'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useText } from '$/providers/react'
import { useSuspenseQuery } from '@tanstack/react-query'
import {
  ListBox,
  ListBoxItem,
  Tag,
  TagGroup,
  TagList,
  useFilter,
  type Selection,
} from 'react-aria-components'
import ConfirmDeleteModal from './ConfirmDeleteModal'

/** Props for a {@link ManageLabelsModal}. */
export interface ManageLabelsModalProps<Asset extends AnyAsset = AnyAsset> {
  readonly backend: Backend
  readonly item: Asset
  readonly triggerRef?: React.MutableRefObject<HTMLElement | null>
}

/**
 * A modal to select labels for an asset.
 * @throws {Error} when the current backend is the local backend, or when the user is offline.
 * This should never happen, as this modal should not be accessible in either case.
 */
export default function ManageLabelsModal<Asset extends AnyAsset = AnyAsset>(
  props: ManageLabelsModalProps<Asset>,
) {
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

/**
 * Form for {@link ManageLabelsModal}.
 */
function ManageLabelsForm(props: ManageLabelsModalProps) {
  const { backend, item } = props

  const { getText } = useText()
  const { data: allLabels } = useSuspenseQuery(backendQueryOptions(backend, 'listTags', []))
  const leastUsedColor = findLeastUsedColor(allLabels)

  const itemLabels =
    item.labels
      ?.map((labelName) => allLabels.find((label) => label.value === labelName))
      .filter((label) => label !== undefined) ?? []

  const styles = MANAGE_LABELS_MODAL_STYLES()

  const createTag = useMutationCallback(backendMutationOptions(backend, 'createTag'))
  const associateTag = useMutationCallback(backendMutationOptions(backend, 'associateTag'))
  const deleteTag = useMutationCallback(backendMutationOptions(backend, 'deleteTag'))

  const createLabel = useEventCallback(async (name: string, color?: LChColor) => {
    const labelName = LabelName(name)
    const newLabel = await createTag([{ value: labelName, color: color ?? leastUsedColor }])

    form.setValue('labels', [...selectedLabels, newLabel.id])
  })

  const deleteLabel = useEventCallback((label: Label) => {
    form.setValue(
      'labels',
      selectedLabels.filter((id) => id !== label.id),
    )
    return deleteTag([label.id, label.value])
  })

  const form = Form.useForm({
    schema: (z) =>
      z.object({
        labels: z.string().array().readonly(),
        query: z.string(),
      }),
    onChange: (name) => {
      if (name === 'labels') {
        const value = form.getValues('labels')

        form.clearErrors()

        const labelNames = value
          .map((label) => allLabels.find((l) => l.id === label)?.value)
          .filter((label) => label !== undefined)

        void associateTag([item.id, labelNames, item.title]).catch(() => {
          form.setFormError(getText('arbitraryMutationError'))
          form.resetField('labels')
        })
      }
    },
    defaultValues: { labels: itemLabels.map((label) => label.id), query: '' },
  })

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
                  .map((label) => allLabels.find((allLabelsItem) => allLabelsItem.id === label))
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
                  labels={allLabels}
                  selectedLabels={field.value}
                  defaultSelectedKeys={itemLabels.map((label) => label.id)}
                  onSelectionChange={(keys) => {
                    const newLabels = (() => {
                      if (keys instanceof Set) {
                        return Array.from(keys)
                          .map((key) => allLabels.find((label) => label.id === key))
                          .filter((label) => label !== undefined)
                      }

                      return allLabels
                    })()

                    field.onChange(newLabels.map((label) => label.id))
                  }}
                  query={query}
                  onCreateLabel={createLabel}
                  onDeleteLabel={deleteLabel}
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

/**
 * Props for a {@link AllLabels}.
 */
interface AllLabelsProps {
  readonly colors: readonly LChColor[]
  readonly leastUsedColor: LChColor
  readonly labels: readonly Label[]
  readonly selectedLabels: readonly string[]
  readonly query: string
  readonly defaultSelectedKeys: readonly string[]
  readonly onSelectionChange: (keys: Selection) => void
  readonly onCreateLabel: (name: string, color: LChColor) => Promise<void>
  readonly onDeleteLabel: (label: Label) => Promise<void>
}

/**
 * A list of all labels.
 */
function AllLabels(props: AllLabelsProps) {
  const {
    labels,
    query,
    onCreateLabel,
    onDeleteLabel,
    colors,
    leastUsedColor,
    defaultSelectedKeys,
    selectedLabels,
    onSelectionChange,
  } = props

  const { getText } = useText()

  const filter = useFilter({ sensitivity: 'base' })

  const filteredLabels = labels.filter((label) => filter.contains(label.value, query))

  return (
    <AnimatedBackground>
      <ListBox
        items={filteredLabels}
        className="flex max-h-72 flex-col overflow-y-auto overflow-x-hidden scroll-offset-edge-0"
        aria-label={getText('manageLabelsModal.allLabels')}
        selectionMode="multiple"
        selectedKeys={selectedLabels}
        defaultSelectedKeys={defaultSelectedKeys}
        onSelectionChange={onSelectionChange}
        dependencies={[query]}
        renderEmptyState={() => (
          <NotFoundLabel
            query={query}
            onCreateLabel={onCreateLabel}
            leastUsedColor={leastUsedColor}
            colors={colors}
          />
        )}
      >
        {(label) => (
          <ListBoxItem
            key={label.id}
            id={label.id}
            textValue={label.value}
            className="group rounded-3xl pressed:bg-primary/5"
          >
            {({ isSelected, isPressed, isHovered }) => (
              <AnimatedBackground.Item
                className="flex w-full items-center gap-2 px-2 py-0.5"
                isSelected={isHovered || isPressed}
                animationClassName="bg-primary/5 rounded-3xl"
              >
                <Check isSelected={isSelected} isPressed={isPressed} />

                <div
                  className="aspect-square w-4 flex-none rounded-full"
                  style={{ backgroundColor: lChColorToCssColor(label.color) }}
                />

                <Text truncate nowrap textSelection="none">
                  {label.value}
                </Text>

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
              </AnimatedBackground.Item>
            )}
          </ListBoxItem>
        )}
      </ListBox>
    </AnimatedBackground>
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

      <Form.Submit form={form} variant="icon" size="small">
        {getText('manageLabelsModal.createLabelWithTitle', query)}
      </Form.Submit>
    </Button.Group>
  )
}
