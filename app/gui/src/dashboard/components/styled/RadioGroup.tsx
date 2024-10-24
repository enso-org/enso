/**
 * @file A copy of `RadioGroup` from `react-aria-components`, with the sole difference being that
 * `onKeyDown` is omitted from `useRadioGroup`.
 */
// NOTE: Some of `react-aria-components/utils.ts` has also been inlined, in order to avoid needing
// to export them, and by extension polluting auto-imports.
/*
 * Copyright 2022 Adobe. All rights reserved.
 * This file is licensed to you under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License. You may obtain a copy
 * of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under
 * the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR REPRESENTATIONS
 * OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License.
 */

import * as React from 'react'

import * as reactStately from 'react-stately'

import * as aria from '#/components/aria'
import { forwardRef } from '#/utilities/react'

/** Options for {@link useRenderProps}. */
interface RenderPropsHookOptions<T> extends aria.DOMProps, aria.AriaLabelingProps {
  /** The CSS [className](https://developer.mozilla.org/en-US/docs/Web/API/Element/className) for the element. A function may be provided to compute the class based on component state. */
  readonly className?: string | ((values: T) => string)
  /** The inline [style](https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/style) for the element. A function may be provided to compute the style based on component state. */
  readonly style?: React.CSSProperties | ((values: T) => React.CSSProperties)
  /** The children of the component. A function may be provided to alter the children based on component state. */
  readonly children?: React.ReactNode | ((values: T) => React.ReactNode)
  readonly values: T
  readonly defaultChildren?: React.ReactNode
  readonly defaultClassName?: string
}

/** Run each render prop if if is a function, otherwise return the value itself. */
function useRenderProps<T>(props: RenderPropsHookOptions<T>) {
  const { className, style, children, defaultClassName, defaultChildren, values } = props

  return React.useMemo(() => {
    let computedClassName: string | undefined
    let computedStyle: React.CSSProperties | undefined
    let computedChildren: React.ReactNode | undefined

    if (typeof className === 'function') {
      computedClassName = className(values)
    } else {
      computedClassName = className
    }

    if (typeof style === 'function') {
      computedStyle = style(values)
    } else {
      computedStyle = style
    }

    if (typeof children === 'function') {
      computedChildren = children(values)
    } else if (children == null) {
      computedChildren = defaultChildren
    } else {
      computedChildren = children
    }

    return {
      className: computedClassName ?? defaultClassName,
      style: computedStyle,
      children: computedChildren,
      // eslint-disable-next-line @typescript-eslint/naming-convention
      'data-rac': '',
    }
  }, [className, style, children, defaultClassName, defaultChildren, values])
}

/** Create a slot. */
function useSlot(): [React.RefCallback<Element>, boolean] {
  // Assume we do have the slot in the initial render.
  const [hasSlot, setHasSlot] = React.useState(true)
  const hasRun = React.useRef(false)

  // A callback ref which will run when the slotted element mounts.
  // This should happen before the useLayoutEffect below.
  const ref = React.useCallback((el: unknown) => {
    hasRun.current = true
    setHasSlot(Boolean(el))
  }, [])

  // If the callback hasn't been called, then reset to false.
  React.useLayoutEffect(() => {
    if (!hasRun.current) {
      setHasSlot(false)
    }
  }, [])

  return [ref, hasSlot]
}

const UNDEFINED = undefined

/** A radio group allows a user to select a single item from a list of mutually exclusive options. */
export default forwardRef(RadioGroup)

/** A radio group allows a user to select a single item from a list of mutually exclusive options. */
function RadioGroup(props: aria.RadioGroupProps, ref: React.ForwardedRef<HTMLDivElement>) {
  ;[props, ref] = aria.useContextProps(props, ref, aria.RadioGroupContext)
  const state = reactStately.useRadioGroupState({
    ...props,
    validationBehavior: props.validationBehavior ?? 'native',
  })

  const [labelRef, label] = useSlot()
  const { radioGroupProps, labelProps, descriptionProps, errorMessageProps, ...validation } =
    aria.useRadioGroup(
      {
        ...props,
        label,
        validationBehavior: props.validationBehavior ?? 'native',
      },
      state,
    )
  // This single line is the reason this file exists!
  delete radioGroupProps.onKeyDown

  const renderProps = useRenderProps({
    ...props,
    defaultClassName: 'react-aria-RadioGroup',
    values: {
      orientation: props.orientation || 'vertical',
      isInvalid: state.isInvalid,
      isDisabled: state.isDisabled,
      isReadOnly: state.isReadOnly,
      isRequired: state.isRequired,
      state,
      defaultClassName: 'react-aria-RadioGroup',
      defaultStyle: {},
      defaultChildren: null,
    },
  })

  return (
    <div
      {...radioGroupProps}
      {...renderProps}
      ref={ref}
      slot={(props.slot ?? '') || UNDEFINED}
      data-orientation={props.orientation || 'vertical'}
      data-invalid={state.isInvalid || UNDEFINED}
      data-disabled={state.isDisabled || UNDEFINED}
      data-readonly={state.isReadOnly || UNDEFINED}
      data-required={state.isRequired || UNDEFINED}
    >
      <aria.Provider
        values={[
          [aria.RadioGroupStateContext, state],
          [aria.LabelContext, { ...labelProps, ref: labelRef, elementType: 'span' }],
          [
            aria.TextContext,
            {
              slots: {
                description: descriptionProps,
                errorMessage: errorMessageProps,
              },
            },
          ],
          [aria.FieldErrorContext, validation],
        ]}
      >
        {renderProps.children}
      </aria.Provider>
    </div>
  )
}
