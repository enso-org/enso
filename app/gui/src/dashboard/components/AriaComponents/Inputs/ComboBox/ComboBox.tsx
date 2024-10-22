/** @file A combo box with a list of items that can be filtered. */
import { useContext, useMemo, type ForwardedRef } from 'react'

import CrossIcon from '#/assets/cross.svg'
import ArrowIcon from '#/assets/folder_arrow.svg'
import {
  ComboBox as AriaComboBox,
  ComboBoxStateContext,
  Label,
  ListBox,
  ListBoxItem,
  type ComboBoxProps as AriaComboBoxProps,
} from '#/components/aria'
import {
  Button,
  Form,
  Input,
  Popover,
  Text,
  type FieldComponentProps,
  type FieldPath,
  type FieldProps,
  type FieldStateProps,
  type FieldValues,
  type InputProps,
  type TSchema,
} from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'
import { forwardRef } from '#/utilities/react'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { tv } from '#/utilities/tailwindVariants'

const POPOVER_CROSS_OFFSET_PX = -32

const COMBO_BOX_STYLES = tv({
  base: 'w-full',
  slots: {
    inputContainer: 'flex items-center gap-2 px-1.5',
    input: 'grow',
    resetButton: '',
    popover: 'py-2',
    listBox: 'text-primary text-xs',
    listBoxItem: 'min-w-min cursor-pointer rounded-full hover:bg-hover-bg px-2',
  },
  defaultVariants: {
    size: 'medium',
  },
})

/** Props for a {@link ComboBox}. */
export interface ComboBoxProps<Schema extends TSchema, TFieldName extends FieldPath<Schema>>
  extends FieldStateProps<
      Omit<
        AriaComboBoxProps<FieldValues<Schema>[TFieldName]>,
        'children' | 'className' | 'style'
      > & { value?: FieldValues<Schema>[TFieldName] },
      Schema,
      TFieldName
    >,
    FieldProps,
    Pick<FieldComponentProps<Schema>, 'className' | 'style'>,
    VariantProps<typeof COMBO_BOX_STYLES>,
    Pick<InputProps<Schema, TFieldName>, 'placeholder'> {
  /** This may change as the user types in the input. */
  readonly items: readonly FieldValues<Schema>[TFieldName][]
  readonly children: (item: FieldValues<Schema>[TFieldName]) => string
  readonly noResetButton?: boolean
}

/** A combo box with a list of items that can be filtered. */
export const ComboBox = forwardRef(function ComboBox<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema>,
>(props: ComboBoxProps<Schema, TFieldName>, ref: ForwardedRef<HTMLFieldSetElement>) {
  const {
    name,
    items,
    isDisabled,
    form,
    defaultValue,
    label,
    isRequired,
    className,
    placeholder,
    children,
    noResetButton = false,
    variants = COMBO_BOX_STYLES,
  } = props
  const itemsAreStrings = typeof items[0] === 'string'
  const effectiveItems = useMemo(
    () => (itemsAreStrings ? items.map((id) => ({ id })) : items),
    [items, itemsAreStrings],
  )

  const { fieldState, formInstance } = Form.useField({
    name,
    isDisabled,
    form,
    defaultValue,
  })

  const styles = variants({})

  return (
    <Form.Field
      form={formInstance}
      name={name}
      fullWidth
      label={label}
      aria-label={props['aria-label']}
      aria-labelledby={props['aria-labelledby']}
      aria-describedby={props['aria-describedby']}
      isRequired={isRequired}
      isInvalid={fieldState.invalid}
      aria-details={props['aria-details']}
      ref={ref}
      style={props.style}
    >
      <Form.Controller
        control={formInstance.control}
        name={name}
        render={(renderProps) => {
          return (
            <AriaComboBox
              className={styles.base({ className })}
              // @ts-expect-error Items must not be strings; this is a limitation of `react-aria`.
              items={effectiveItems}
              {...renderProps.field}
              onSelectionChange={(key) => {
                renderProps.field.onChange(key ?? '')
              }}
            >
              <Label>{label}</Label>
              <div className={styles.inputContainer()}>
                <Button variant="icon" icon={ArrowIcon} className="rotate-90" />
                <Input
                  name={name}
                  placeholder={placeholder}
                  size="custom"
                  variant="custom"
                  value={renderProps.field.value}
                />
                {!noResetButton && <ComboBoxResetButton className={styles.resetButton()} />}
              </div>
              <Popover crossOffset={POPOVER_CROSS_OFFSET_PX} className={styles.popover()}>
                <ListBox className={styles.listBox()}>
                  {(item) => {
                    const text = children(
                      // @ts-expect-error When items are strings, they are mapped to
                      // `{ id: item }`.
                      // eslint-disable-next-line no-restricted-syntax
                      (itemsAreStrings ? item.id : item) as FieldValues<Schema>[TFieldName],
                    )
                    return (
                      <ListBoxItem id={text} className={styles.listBoxItem()}>
                        <Text truncate="1" className="w-full" tooltipPlacement="left">
                          {text}
                        </Text>
                      </ListBoxItem>
                    )
                  }}
                </ListBox>
              </Popover>
            </AriaComboBox>
          )
        }}
      />
    </Form.Field>
  )
})

/** Props for a {@link ComboBoxResetButton}. */
interface ComboBoxResetButtonProps {
  readonly className?: string
}

/** A reset button for a {@link ComboBox}. */
function ComboBoxResetButton(props: ComboBoxResetButtonProps) {
  const { className } = props
  const state = useContext(ComboBoxStateContext)
  const { getText } = useText()

  return (
    <Button
      // Do not inherit default `Button` behavior from `ComboBox`.
      slot={null}
      variant="icon"
      aria-label={getText('reset')}
      icon={CrossIcon}
      className={className ?? ''}
      onPress={() => {
        state.setInputValue('')
      }}
    />
  )
}
