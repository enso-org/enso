/** @file A combo box with a list of items that can be filtered. */
import CrossIcon from '#/assets/cross.svg'
import ArrowIcon from '#/assets/folder_arrow.svg'
import {
  ComboBox as AriaComboBox,
  ComboBoxStateContext,
  ListBox,
  ListBoxItem,
  type ComboBoxProps as AriaComboBoxProps,
} from '#/components/aria'
import { Button } from '#/components/Button'
import { Popover } from '#/components/Dialog'
import {
  Form,
  type FieldComponentProps,
  type FieldPath,
  type FieldProps,
  type FieldStateProps,
  type FieldValues,
  type TSchema,
} from '#/components/Form'
import { BasicInput, type InputProps } from '#/components/Inputs/Input'
import { Text } from '#/components/Text'
import { makeRoundedStyles } from '#/components/utilities'
import { VisualTooltip } from '#/components/VisualTooltip'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { tv } from '#/utilities/tailwindVariants'
import { useText } from '$/providers/react'
import { forwardRef, useContext, useRef, type ForwardedRef, type ReactNode } from 'react'
import invariant from 'tiny-invariant'

const COMBO_BOX_STYLES = tv({
  base: 'w-full',
  variants: {
    rounded: makeRoundedStyles('inputContainer'),
    size: {
      custom: '',
      small: { inputContainer: 'px-[11px] pb-0.5 pt-1' },
      medium: { inputContainer: 'px-[11px] pb-[6.5px] pt-[8.5px]' },
    },
  },
  slots: {
    inputContainer: 'flex items-center gap-2 px-1.5 rounded-full border-0.5 border-primary/20',
    input: 'grow',
    resetButton: '',
    popover: 'py-2 w-[calc(var(--trigger-width)_+_48px)]',
    listBox: 'text-primary text-xs',
    listBoxItem: 'cursor-pointer rounded-full hover:bg-hover-bg px-2',
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
    Pick<InputProps<Schema, TFieldName, string>, 'placeholder'> {
  /** This may change as the user types in the input. */
  readonly items: readonly FieldValues<Schema>[TFieldName][]
  /** A text-like representation of the item to be shown on each option. */
  readonly children: (item: FieldValues<Schema>[TFieldName]) => ReactNode
  /**
   * Convert an item to a unique text id, if the default text format returned by
   * `children` is not guaranteed (or not supposed) to be unique.
   */
  readonly toKey?: (item: FieldValues<Schema>[TFieldName]) => string
  /** Convert an item to a text value for filtering and typeahead. */
  readonly toTextValue?: (item: FieldValues<Schema>[TFieldName]) => string
  /** Convert an item to the tooltip to be shown, if different from the item itself. */
  readonly toTooltip?: (item: FieldValues<Schema>[TFieldName]) => string
  /** Hide the `x` button to disable resetting the input. */
  readonly noResetButton?: boolean
  readonly addonStart?:
    | InputProps<Schema, TFieldName, string>['addonStart']
    | ((
        item: FieldValues<Schema>[TFieldName],
      ) => InputProps<Schema, TFieldName, string>['addonStart'])
  readonly addonEnd?:
    | InputProps<Schema, TFieldName, string>['addonEnd']
    | ((
        item: FieldValues<Schema>[TFieldName],
      ) => InputProps<Schema, TFieldName, string>['addonEnd'])
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
    toKey = toTextValue,
    toTooltip,
    noResetButton = false,
    variants = COMBO_BOX_STYLES,
    contextualHelp,
    addonStart,
    addonEnd,
  } = props
  const itemsAreStrings = typeof items[0] === 'string'
  const effectiveItems = itemsAreStrings ? items.map((id) => ({ id })) : items
  const reverseMapping = new Map(
    items.map((item) => {
      const childrenEl = children(item)
      const textValue =
        toKey?.(item) ??
        (typeof childrenEl === 'string' ? childrenEl
        : typeof item === 'string' ? item
        : null)
      invariant(
        textValue != null,
        'Every element in a `ComboBox` must have a string representation',
      )
      return [textValue, item]
    }),
  )
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
      description={props.description}
      aria-describedby={props['aria-describedby']}
      isRequired={isRequired}
      isInvalid={fieldState.invalid}
      aria-details={props['aria-details']}
      ref={ref}
      contextualHelp={contextualHelp}
      style={props.style}
    >
      <Form.Controller
        control={formInstance.control}
        name={name}
        render={(renderProps) => (
          <AriaComboBox
            key={renderProps.field.value}
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
                addonStart={
                  typeof addonStart === 'function' ?
                    addonStart(renderProps.field.value)
                  : addonStart
                }
                addonEnd={
                  typeof addonEnd === 'function' ? addonEnd(renderProps.field.value) : addonEnd
                }
                size="custom"
                variant="custom"
              />
              {!noResetButton && <ComboBoxResetButton className={styles.resetButton()} />}
            </div>
            <Popover
              triggerRef={popoverTriggerRef}
              size="auto-xxsmall"
              className={styles.popover()}
            >
              <ListBox aria-label={props['aria-label'] ?? 'Combo box'} className={styles.listBox()}>
                {(item) => {
                  // eslint-disable-next-line no-restricted-syntax
                  const fieldValue = (
                    itemsAreStrings ?
                      // @ts-expect-error When items are strings, they are mapped to
                      // `{ id: item }`.
                      item.id
                    : item) as FieldValues<Schema>[TFieldName]
                  const childrenEl = children(fieldValue)
                  const textValue =
                    toTextValue?.(fieldValue) ??
                    (typeof childrenEl === 'string' ? childrenEl
                    : typeof fieldValue === 'string' ? fieldValue
                    : null)
                  const key =
                    toKey?.(fieldValue) ??
                    (typeof childrenEl === 'string' ? childrenEl
                    : typeof fieldValue === 'string' ? fieldValue
                    : null)
                  invariant(
                    textValue != null && key != null,
                    'Every element in a `ComboBox` must have a string representation',
                  )
                  const tooltip = toTooltip?.(fieldValue) ?? textValue

                  return (
                    <ListBoxItem id={key} textValue={textValue} className={styles.listBoxItem()}>
                      {typeof childrenEl === 'string' ?
                        <Text
                          truncate="1"
                          className="w-full"
                          tooltip={toTooltip ? tooltip : childrenEl}
                          tooltipPlacement="left"
                        >
                          {childrenEl}
                        </Text>
                      : <VisualTooltip tooltip={tooltip} className="flex w-full">
                          {childrenEl}
                        </VisualTooltip>
                      }
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
