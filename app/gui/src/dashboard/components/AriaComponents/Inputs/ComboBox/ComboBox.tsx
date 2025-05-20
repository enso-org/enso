/** @file A combo box with a list of items that can be filtered. */
import { forwardRef, useContext, useRef, type ForwardedRef } from 'react'

import CrossIcon from '#/assets/cross.svg'
import ArrowIcon from '#/assets/folder_arrow.svg'
import {
  ComboBox as AriaComboBox,
  ComboBoxStateContext,
  ListBox,
  ListBoxItem,
  type ComboBoxProps as AriaComboBoxProps,
} from '#/components/aria'
import { useText } from '#/providers/TextProvider'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { tv } from '#/utilities/tailwindVariants'
import {
  BasicInput,
  Button,
  Form,
  Popover,
  Text,
  type FieldComponentProps,
  type FieldPath,
  type FieldProps,
  type FieldStateProps,
  type FieldValues,
  type InputProps,
  type TSchema,
} from '../..'
// This cannot be added to the import above or else it is `undefined` due to a circular import.
import { makeRoundedStyles } from '../../utilities'

const COMBO_BOX_STYLES = tv({
  base: 'w-full',
  variants: {
    rounded: makeRoundedStyles('inputContainer'),
    size: {
      small: {
        inputContainer: 'h-6 px-2',
      },
      medium: {
        inputContainer: 'h-8 px-4',
      },
    },
  },
  slots: {
    inputContainer: 'flex items-center gap-2 px-1.5 rounded-full border-0.5 border-primary/20',
    input: 'grow',
    resetButton: '',
    popover: 'py-2',
    listBox: 'text-primary text-xs',
    listBoxItem: 'min-w-min cursor-pointer rounded-full hover:bg-hover-bg px-2',
  },
  defaultVariants: {
    size: 'medium',
    rounded: 'xlarge',
  },
})

/** Props for a {@link ComboBox}. */
export interface ComboBoxProps<Schema extends TSchema, TFieldName extends FieldPath<Schema, string>>
  extends FieldStateProps<
      Omit<
        AriaComboBoxProps<FieldValues<Schema>[TFieldName]>,
        'children' | 'className' | 'style'
      > & { value?: FieldValues<Schema>[TFieldName] },
      Schema,
      TFieldName,
      string
    >,
    FieldProps,
    Pick<FieldComponentProps<Schema>, 'className' | 'style'>,
    VariantProps<typeof COMBO_BOX_STYLES>,
    Pick<InputProps<Schema, TFieldName, string>, 'addonEnd' | 'addonStart' | 'placeholder'> {
  /** This may change as the user types in the input. */
  readonly items: readonly FieldValues<Schema>[TFieldName][]
  /** A text representation of the item to be shown on each option. */
  readonly children: (item: FieldValues<Schema>[TFieldName]) => string
  /**
   * Convert an item to a unique text id, if the default text format returned by
   * `children` is not guaranteed (or not supposed) to be unique.
   */
  readonly toTextValue?: (item: FieldValues<Schema>[TFieldName]) => string
  /** Hide the `x` button to disable resetting the input. */
  readonly noResetButton?: boolean
}

// This is a function, even though it does not contain function syntax.
// eslint-disable-next-line no-restricted-syntax
const useStringField = Form.makeUseField<string>()

/** A combo box with a list of items that can be filtered. */
export const ComboBox = forwardRef(function ComboBox<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, string>,
>(props: ComboBoxProps<Schema, TFieldName>, ref: ForwardedRef<HTMLDivElement>) {
  const {
    name,
    items,
    isDisabled,
    form,
    defaultValue,
    defaultInputValue,
    defaultSelectedKey,
    label,
    isRequired,
    className,
    placeholder,
    size,
    rounded,
    children,
    toTextValue,
    noResetButton = false,
    variants = COMBO_BOX_STYLES,
    addonStart,
    addonEnd,
  } = props
  const itemsAreStrings = typeof items[0] === 'string'
  const effectiveItems = itemsAreStrings ? items.map((id) => ({ id })) : items
  const toTextValueOrText = toTextValue ?? children
  const reverseMapping = new Map(items.map((item) => [toTextValueOrText(item), item]))
  const popoverTriggerRef = useRef<HTMLDivElement>(null)

  const { fieldState, formInstance } = useStringField({
    name,
    isDisabled,
    form,
    defaultValue,
  })

  const styles = variants({ size, rounded })

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
        render={(renderProps) => (
          <AriaComboBox
            aria-label={props['aria-label'] ?? 'Combo box'}
            className={styles.base({ className })}
            // @ts-expect-error Items must not be strings; this is a limitation of `react-aria`.
            defaultItems={effectiveItems}
            {...renderProps.field}
            defaultInputValue={defaultInputValue}
            defaultSelectedKey={defaultSelectedKey ?? renderProps.field.value}
            onSelectionChange={(key) => {
              renderProps.field.onChange(typeof key === 'string' ? reverseMapping.get(key) : null)
            }}
          >
            <div ref={popoverTriggerRef} className={styles.inputContainer()}>
              <Button variant="icon" icon={ArrowIcon} className="rotate-90" />
              <BasicInput
                name={name}
                placeholder={placeholder}
                addonStart={addonStart}
                addonEnd={addonEnd}
                size="custom"
                variant="custom"
              />
              {!noResetButton && <ComboBoxResetButton className={styles.resetButton()} />}
            </div>
            <Popover triggerRef={popoverTriggerRef} className={styles.popover()}>
              <ListBox aria-label={props['aria-label'] ?? 'Combo box'} className={styles.listBox()}>
                {(item) => {
                  // eslint-disable-next-line no-restricted-syntax
                  const fieldValue = (
                    itemsAreStrings ?
                      // @ts-expect-error When items are strings, they are mapped to
                      // `{ id: item }`.
                      item.id
                    : item) as FieldValues<Schema>[TFieldName]
                  const text = children(fieldValue)
                  const textValue = toTextValue?.(fieldValue) ?? text
                  return (
                    <ListBoxItem
                      id={textValue}
                      textValue={textValue}
                      className={styles.listBoxItem()}
                    >
                      <Text truncate="1" className="w-full" tooltipPlacement="left">
                        {text}
                      </Text>
                    </ListBoxItem>
                  )
                }}
              </ListBox>
            </Popover>
          </AriaComboBox>
        )}
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
        state?.setInputValue('')
      }}
    />
  )
}
