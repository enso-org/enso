/** @file A radio button. */
import * as aria from '#/components/aria'
import { mergeRefs } from '#/utilities/mergeRefs'
import { tv } from '#/utilities/tailwindVariants'
import * as React from 'react'
import invariant from 'tiny-invariant'
import * as text from '../Text'
import { RadioGroup } from './RadioGroup'
import { useRadioGroupContext } from './RadioGroupContext'

const RADIO_STYLES = tv({
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

/** Props for the {@link Radio} component. */
export interface RadioProps extends aria.RadioProps {
  readonly label?: string
}

/** A radio button. */
export const Radio = Object.assign(
  React.forwardRef(function Radio(props: RadioProps, ref: React.ForwardedRef<HTMLLabelElement>) {
    const { children, label, className, ...ariaProps } = props

    const inputRef = React.useRef<HTMLInputElement>(null)
    const labelRef = React.useRef<HTMLLabelElement>(null)
    const id = aria.useId(ariaProps.id)

    const state = React.useContext(aria.RadioGroupStateContext)
    const { setPressed, clearPressed, isSiblingPressed } = useRadioGroupContext({
      value: props.value,
    })

    invariant(state, '<Radio /> must be used within a <RadioGroup />')

    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
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
        // eslint-disable-next-line @typescript-eslint/no-unsafe-argument
        {...aria.mergeProps<React.LabelHTMLAttributes<HTMLLabelElement>>()(hoverProps, labelProps)}
        ref={(el) => {
          mergeRefs(labelRef, ref)(el)
        }}
        className={base()}
      >
        <input
          {...aria.mergeProps<React.InputHTMLAttributes<HTMLInputElement>>()(
            // eslint-disable-next-line @typescript-eslint/no-unsafe-argument
            inputProps,
            focusProps,
          )}
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
  }),
  /* eslint-disable @typescript-eslint/naming-convention */
  {
    Group: RadioGroup,
  },
  /* eslint-enable @typescript-eslint/naming-convention */
)
