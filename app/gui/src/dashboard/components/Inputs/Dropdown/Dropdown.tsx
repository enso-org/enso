/** @file A styled dropdown. */
import { forwardRef, useEffect, useRef, useState, type ForwardedRef, type ReactNode } from 'react'

import CheckMarkIcon from '#/assets/check_mark.svg'
import ArrowIcon from '#/assets/folder_arrow.svg'
import {
  FieldError,
  ListBox,
  ListBoxItem,
  mergeProps,
  useFocusVisible,
  useFocusWithin,
  type InputProps,
} from '#/components/aria'
import FocusRing from '#/components/styled/FocusRing'
import SvgMask from '#/components/SvgMask'
import { useSyncRef } from '#/hooks/syncRefHooks'
import { mergeRefs } from '#/utilities/mergeRefs'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import { DIALOG_BACKGROUND } from '../../Dialog/variants'
import {
  Form,
  type FieldComponentProps,
  type FieldPath,
  type FieldProps,
  type FieldStateProps,
  type FieldValues,
  type FieldVariantProps,
  type FormInstance,
  type TSchema,
} from '../../Form'
import { makeRoundedStyles } from '../../utilities'

const DROPDOWN_STYLES = tv({
  base: 'group relative flex w-max cursor-pointer flex-col items-start whitespace-nowrap rounded-input',
  variants: {
    isFocused: {
      true: {
        container: 'z-1',
        options: DIALOG_BACKGROUND({
          className: 'shadow-xl overflow-hidden rounded-input',
        }),
        optionsContainer: 'grid-rows-1fr',
        input: 'z-1',
      },
      false: {
        container: 'overflow-hidden',
        options: 'before:h-full',
        optionsContainer: 'grid-rows-0fr',
      },
    },
    isReadOnly: {
      true: {
        input: 'read-only',
      },
    },
    multiple: {
      true: {
        optionsItem: 'hover:font-semibold',
      },
    },
    rounded: makeRoundedStyles('options', (classes) => `before:${classes}`),
    size: {
      medium: {
        container: 'h-10',
        input: 'px-4 pb-[6.5px] pt-[8.5px] h-10',
        optionsItem: 'px-4',
        hiddenOption: 'px-4',
        icon: 'size-4',
      },
      small: {
        container: 'h-8',
        input: 'px-4 py-1',
        optionsItem: 'px-4',
        hiddenOption: 'px-4',
        icon: 'size-3',
      },
      custom: {},
    },
  },
  slots: {
    container: 'absolute inset-0 min-h-full w-full min-w-max pb-px',
    icon: '',
    options:
      'relative min-h-full before:absolute before:inset-0 before:h-full before:w-full before:rounded-input before:border-0.5 before:border-primary/20 before:transition-colors',
    optionsSpacing: 'padding relative h-full',
    optionsContainer: 'relative grid max-h-60 w-full overflow-auto transition-grid-template-rows',
    optionsList: 'overflow-auto',
    optionsItem:
      'flex min-h-6 items-center gap-2 rounded-input transition-colors focus:cursor-default focus:bg-frame focus:font-bold focus:focus-ring not-focus:hover:bg-hover-bg not-selected:hover:bg-hover-bg',
    input: 'group relative flex items-center gap-2 w-full',
    dropdownArrow: 'rotate-90 opacity-80 group-hover:opacity-100',
    inputDisplay: 'grow select-none',
    hiddenOptions: 'flex h-0 flex-col overflow-hidden',
    hiddenOption: 'flex gap-2 font-bold',
  },
  defaultVariants: {
    rounded: 'xlarge',
    size: 'medium',
  },
})

/** Props for a list item child. */
interface InternalChildProps<T> {
  readonly item: T
}

/** Props for the display of the currently selected item, when the dropdown supports multiple children. */
interface InternalChildrenProps<T> {
  readonly items: readonly T[]
  /** This is the value passed as {@link DropdownProps.children}. */
  readonly children: (props: InternalChildProps<T>) => ReactNode
}

/** Props for a {@link Dropdown} shared between all variants. */
interface InternalBaseDropdownProps<T>
  extends InternalChildrenProps<T>,
    Omit<VariantProps<typeof DROPDOWN_STYLES>, 'isFocused' | 'isReadOnly' | 'multiple'> {
  readonly readOnly?: boolean
  readonly className?: string
  // eslint-disable-next-line @typescript-eslint/naming-convention
  readonly 'aria-label'?: string | undefined
}

/** Props for a {@link Dropdown}, when `multiple` is `false` or absent. */
interface InternalSingleDropdownProps<T> extends InternalBaseDropdownProps<T> {
  readonly multiple?: false
  readonly selectedIndex: number | null
  readonly onChange: (item: T, index: number) => void
}

/** Props for a {@link Dropdown}, when `multiple` is `true`. */
interface InternalMultipleDropdownProps<T> extends InternalBaseDropdownProps<T> {
  readonly multiple: true
  readonly selectedIndices: readonly number[]
  readonly renderMultiple: (props: InternalChildrenProps<T>) => ReactNode
  readonly onChange: (items: readonly T[], indices: readonly number[]) => void
}

/** Props for a {@link Dropdown}. */
export type DropdownProps<T> = InternalMultipleDropdownProps<T> | InternalSingleDropdownProps<T>

/** A styled dropdown. */
export const Dropdown = forwardRef(function Dropdown<T>(
  props: DropdownProps<T>,
  ref: ForwardedRef<HTMLDivElement>,
) {
  const {
    readOnly = false,
    className,
    items,
    rounded,
    size,
    variants = DROPDOWN_STYLES,
    children: Child,
  } = props
  const listBoxItems = items.map((item, i) => ({ item, i }))
  const [tempSelectedIndex, setTempSelectedIndex] = useState<number | null>(null)
  const rootRef = useRef<HTMLDivElement | null>(null)
  const [isFocusWithin, setIsFocusWithin] = useState(false)
  const [isMouseFocused, setIsMouseFocused] = useState(false)
  const { isFocusVisible } = useFocusVisible()
  const isFocusedRef = useSyncRef(isFocusWithin)
  const isSelfMouseDownRef = useRef(false)
  const delayedIsFocused = useRef(false)
  const { focusWithinProps } = useFocusWithin({
    onFocusWithinChange: setIsFocusWithin,
  })
  const multiple = props.multiple === true
  const selectedIndex = 'selectedIndex' in props ? props.selectedIndex : null
  const selectedIndices =
    'selectedIndices' in props ? props.selectedIndices
    : selectedIndex != null ? [selectedIndex]
    : []
  const selectedItems = selectedIndices.flatMap((index) => {
    const item = items[index]
    return item != null ? [item] : []
  })
  const visuallySelectedIndex = tempSelectedIndex ?? selectedIndex
  const visuallySelectedItem = visuallySelectedIndex == null ? null : items[visuallySelectedIndex]

  const isFocused = isFocusVisible ? isFocusWithin : isMouseFocused

  const styles = variants({
    isFocused,
    isReadOnly: readOnly,
    multiple,
    rounded,
    size,
  })

  useEffect(() => {
    setTempSelectedIndex(selectedIndex)
  }, [selectedIndex])

  useEffect(() => {
    const onDocumentMouseDown = () => {
      if (!isSelfMouseDownRef.current) {
        setIsMouseFocused(false)
        if (document.activeElement === rootRef.current) {
          rootRef.current?.blur()
        }
        isSelfMouseDownRef.current = false
      }
    }
    document.addEventListener('mousedown', onDocumentMouseDown)
    return () => {
      document.removeEventListener('mousedown', onDocumentMouseDown)
    }
  }, [isFocusedRef])

  useEffect(() => {
    const handle = requestAnimationFrame(() => {
      delayedIsFocused.current = isFocused
    })
    return () => {
      cancelAnimationFrame(handle)
    }
  }, [isFocused])

  return (
    <FocusRing placement="outset">
      <div
        ref={(el) => {
          mergeRefs(ref, rootRef)(el)
        }}
        onMouseDown={() => {
          isSelfMouseDownRef.current = true
          // `isFocused` cannot be used as `isFocusWithin` is set to `false` immediately before
          // this event handler is called.
          setIsMouseFocused(!delayedIsFocused.current)
        }}
        tabIndex={-1}
        className={styles.base({ className })}
        {...mergeProps<React.JSX.IntrinsicElements['div']>()(focusWithinProps, {
          onBlur: (event) => {
            if (!event.currentTarget.contains(event.relatedTarget)) {
              setIsMouseFocused(false)
            }
          },
        })}
      >
        <div className={styles.container()}>
          <div className={styles.options()}>
            {/* Spacing. */}
            <div className={styles.input()}>&nbsp;</div>
            <div className={styles.optionsContainer()}>
              <ListBox
                aria-label={props['aria-label'] ?? 'Dropdown'}
                selectionMode={multiple ? 'multiple' : 'single'}
                selectionBehavior={multiple ? 'toggle' : 'replace'}
                items={listBoxItems}
                dependencies={[selectedIndices]}
                className={styles.optionsList()}
                onSelectionChange={(keys) => {
                  if (multiple) {
                    const indices = Array.from(keys, (i) => Number(i))
                    props.onChange(
                      indices.flatMap((i) => {
                        const item = items[i]
                        return item === undefined ? [] : [item]
                      }),
                      indices,
                    )
                  } else {
                    const [key] = keys
                    if (key != null) {
                      const i = Number(key)
                      const item = items[i]
                      if (item !== undefined) {
                        props.onChange(item, i)
                        setIsMouseFocused(false)
                        rootRef.current?.blur()
                      }
                    }
                  }
                }}
              >
                {({ item, i }) => (
                  <ListBoxItem
                    key={i}
                    id={i}
                    textValue={typeof item === 'string' ? item : `${i}`}
                    className={styles.optionsItem()}
                  >
                    <SvgMask
                      src={CheckMarkIcon}
                      className={styles.icon({
                        className: selectedIndices.includes(i) ? '' : 'invisible',
                      })}
                    />
                    <Child item={item} />
                  </ListBoxItem>
                )}
              </ListBox>
            </div>
          </div>
        </div>
        <div className={styles.input()}>
          <SvgMask src={ArrowIcon} className={styles.dropdownArrow()} />
          <div className={styles.inputDisplay()}>
            {isMouseFocused && !multiple ?
              '\u00a0'
            : visuallySelectedItem != null ?
              <Child item={visuallySelectedItem} />
            : multiple && <props.renderMultiple items={selectedItems}>{Child}</props.renderMultiple>
            }
          </div>
        </div>
        {/* Hidden, but required to exist for the width of the parent element to be correct.
         * Classes that do not affect width have been removed. */}
        <div className={styles.hiddenOptions()}>
          {items.map((item, i) => (
            <div key={i} className={styles.hiddenOption()}>
              <SvgMask src={CheckMarkIcon} />
              <Child item={item} />
            </div>
          ))}
        </div>
      </div>
    </FocusRing>
  )
})

/** Props for a {@link FormDropdown}. */
export interface FormDropdownProps<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, Constraint>,
  Constraint,
> extends FieldStateProps<
      Omit<DropdownProps<Constraint>, 'aria-label' | 'multiple' | 'onChange'> & {
        value: FieldValues<Schema>[TFieldName]
      },
      Schema,
      TFieldName,
      Constraint
    >,
    FieldProps,
    FieldVariantProps {
  readonly form?: FormInstance<Schema>
  readonly name: TFieldName
}

/** A dropdown. */
export function FormDropdown<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, Constraint>,
  Constraint,
>(props: FormDropdownProps<Schema, TFieldName, Constraint>) {
  const { name, children, rounded, size, variants, contextualHelp, ...inputProps } = props
  const { items } = inputProps

  const form = Form.useFormContext(props.form)

  const { fieldProps, formInstance } = Form.useFieldRegister<
    Omit<InputProps, 'children' | 'size'>,
    Schema,
    TFieldName,
    Constraint
  >({ ...props, form })

  return (
    <Form.Field
      {...mergeProps<FieldComponentProps<Schema>>()(inputProps, fieldProps, {
        form: formInstance,
      })}
      name={props.name}
      isRequired={props.isRequired}
      contextualHelp={contextualHelp}
    >
      <Form.Controller
        control={form.control}
        name={name}
        render={({ field, fieldState }) => {
          const { value, onChange } = field
          return (
            <>
              <Dropdown
                {...inputProps}
                selectedIndex={items.findIndex(
                  (otherItem) => JSON.stringify(value) === JSON.stringify(otherItem),
                )}
                onChange={onChange}
                rounded={rounded}
                size={size}
                variants={variants}
              >
                {children}
              </Dropdown>
              <FieldError>{fieldState.error?.message}</FieldError>
            </>
          )
        }}
      />
    </Form.Field>
  )
}
