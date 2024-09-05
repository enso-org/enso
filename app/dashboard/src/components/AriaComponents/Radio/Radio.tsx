/**
 * @file
 *
 * A radio button.
 */

import * as React from 'react'

import * as aria from '#/components/aria'

import * as mergeRefs from '#/utilities/mergeRefs'
import * as twv from '#/utilities/tailwindVariants'

import { forwardRef } from '#/utilities/react'
import * as text from '../Text'
import * as radioGroup from './RadioGroup'
import * as radioGroupContext from './RadioGroupContext'

const RADIO_STYLES = twv.tv({
  base: 'flex items-center gap-2 cursor-pointer group w-full',
  variants: {
    isFocused: { true: 'outline-none' },
    isFocusVisible: { true: { radio: 'outline outline-2 outline-primary outline-offset-1' } },
    isSelected: {
      false: { radio: 'border-2 border-primary/30' },
      true: { radio: 'border-primary border-[5px]' },
    },
    isHovered: { true: { radio: 'border-primary/50' } },
    isInvalid: { true: { radio: 'border-danger' } },
    isDisabled: { true: { base: 'cursor-not-allowed', radio: 'border-gray-200' } },
    isPressed: { true: { radio: 'border-[3px] border-primary' } },
    isSiblingPressed: { true: '' },
  },
  slots: {
    radio:
      'w-4 h-4 rounded-full bg-frame aspect-square flex-none transition-[border-color,border-width,outline-offset] duration-50 ease-in-out',
    input: 'sr-only',
    label: 'flex-1 shrink-0',
  },
  compoundVariants: [
    { isPressed: true, isSelected: true, class: { radio: 'border-[5px]' } },
    { isPressed: true, isInvalid: true, class: { radio: 'border-red-800' } },
    { isSiblingPressed: true, isSelected: true, class: { radio: 'border-4' } },
  ],
})

/**
 * Props for the {@link Radio} component.
 */
export interface RadioProps extends aria.RadioProps {
  readonly label?: string
}

/**
 * A radio button.
 */
// eslint-disable-next-line no-restricted-syntax
export const Radio = forwardRef(function Radio(
  props: RadioProps,
  ref: React.ForwardedRef<HTMLLabelElement>,
) {
  const { children, label, className, ...ariaProps } = props

  const inputRef = React.useRef<HTMLInputElement>(null)
  const labelRef = React.useRef<HTMLLabelElement>(null)
  const id = aria.useId(ariaProps.id)

  const state = React.useContext(aria.RadioGroupStateContext)
  const { setPressed, clearPressed, isSiblingPressed } = radioGroupContext.useRadioGroupContext({
    value: props.value,
  })

  const { isSelected, isDisabled, isPressed, inputProps, labelProps } = aria.useRadio(
    aria.mergeProps<aria.RadioProps>()(ariaProps, {
      id,
      children: label ?? (typeof children === 'function' ? true : children),
    }),
    state,
    inputRef,
  )

  const { isFocused, isFocusVisible, focusProps } = aria.useFocusRing()
  const interactionDisabled = isDisabled || state.isReadOnly
  const { hoverProps, isHovered } = aria.useHover({
    ...props,
    isDisabled: interactionDisabled,
  })

  React.useEffect(() => {
    if (isPressed) {
      setPressed()
    } else {
      clearPressed()
    }
  }, [isPressed, setPressed, clearPressed])

  const renderValues = {
    isSelected,
    isPressed,
    isHovered,
    isFocused,
    isFocusVisible,
    isDisabled,
    isReadOnly: state.isReadOnly,
    isInvalid: state.isInvalid,
    isRequired: state.isRequired,
    defaultChildren: null,
    defaultClassName: '',
  }

  const {
    base,
    radio,
    input,
    label: labelClasses,
  } = RADIO_STYLES({
    isSiblingPressed,
    isFocused,
    isFocusVisible,
    isHovered,
    isSelected,
    isInvalid: state.isInvalid,
    isDisabled,
    isPressed,
    className: typeof className === 'function' ? className(renderValues) : className,
  })

  const renderedChildren = typeof children === 'function' ? children(renderValues) : children

  return (
    <label
      {...aria.mergeProps<React.LabelHTMLAttributes<HTMLLabelElement>>()(hoverProps, labelProps)}
      ref={mergeRefs.mergeRefs(labelRef, ref)}
      className={base()}
    >
      <input
        {...aria.mergeProps<React.InputHTMLAttributes<HTMLInputElement>>()(inputProps, focusProps)}
        ref={inputRef}
        id={id}
        className={input()}
      />

      <div className={radio()} />

      <text.Text className={labelClasses()} variant="body" truncate="1">
        {label ?? renderedChildren}
      </text.Text>
    </label>
  )
}) as unknown as ((
  props: RadioProps & React.RefAttributes<HTMLLabelElement>,
  // eslint-disable-next-line no-restricted-syntax
) => React.JSX.Element) & {
  // eslint-disable-next-line @typescript-eslint/naming-convention
  Group: typeof radioGroup.RadioGroup
}

Radio.Group = radioGroup.RadioGroup
