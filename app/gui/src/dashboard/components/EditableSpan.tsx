/** @file A text `<span>` which turns into an `input` when desired. */
import CrossIcon from '#/assets/cross.svg'
import TickIcon from '#/assets/tick.svg'
import { useInteractOutside } from '#/components/aria'
import { Button } from '#/components/Button'
import { Form } from '#/components/Form'
import { Input } from '#/components/Inputs/Input'
import { Text } from '#/components/Text'
import { Underlay } from '#/components/Underlay'
import { useAutoFocus } from '#/hooks/autoFocusHooks'
import { useMeasure } from '#/hooks/measureHooks'
import { twJoin } from '#/utilities/tailwindMerge'
import { useText } from '$/providers/react'
import * as React from 'react'
import { useLayoutEffect } from 'react'
import type { z } from 'zod'

/** Props for {@link EditableSpan}. */
export interface EditableSpanProps {
  readonly 'data-testid'?: string
  readonly className?: string
  readonly editable?: boolean
  readonly onSubmit: (value: string) => Promise<void>
  readonly onCancel: () => void
  readonly children: string
  /** Additional schema to validate the value. */
  readonly schema?: (schema: z.ZodType<string>) => z.ZodType<string>
}

/** A `<span>` that can turn into an `<input type="text">`. */
export default function EditableSpan(props: EditableSpanProps) {
  const { className = '', editable = false, children } = props

  if (!editable) {
    return (
      <Text className={twJoin('min-w-0', className)} testId={props['data-testid']} truncate="1">
        {children}
      </Text>
    )
  }

  return <EditForm {...props} />
}

/** Props for {@link EditForm}. */
interface EditFormProps extends EditableSpanProps {}

/** Edit form for {@link EditableSpan}. */
function EditForm(props: EditFormProps) {
  const { className = '', children, onSubmit, onCancel, schema } = props

  const { getText } = useText()

  const formRef = React.useRef<HTMLFormElement | null>(null)
  const inputRef = React.useRef<HTMLInputElement | null>(null)

  const form = Form.useForm({
    schema: (z) => {
      const baseValueSchema = z.string().min(1).trim()
      const baseSchema = z.object({ value: baseValueSchema })

      if (schema != null) {
        return baseSchema.merge(z.object({ value: schema(baseValueSchema) }))
      }

      return baseSchema
    },
    defaultValues: { value: children },
    onSubmit: ({ value }) => onSubmit(value),
  })

  useInteractOutside({ ref: formRef, onInteractOutside: onCancel })
  useAutoFocus({ ref: inputRef })

  const { error } = Form.useFieldState({ name: 'value', form })
  const formErrors = Form.useFormError({ form })

  const errorMessage = (() => {
    if (error != null) {
      return error
    }

    if (formErrors.length > 0) {
      return formErrors
        .filter(({ type }) => type === 'error')
        .map(({ message }) => message)
        .join('\n')
    }

    return null
  })()

  const hasError = errorMessage != null

  return (
    <form
      ref={formRef}
      className="relative flex grow gap-1.5"
      data-testid="editable-span-form"
      {...form.formProps}
    >
      <Form.Provider form={form}>
        <div className="flex flex-1 flex-shrink-0 basis-full items-center">
          <Input
            inputRef={inputRef}
            name="value"
            variant="custom"
            size="custom"
            rounded="none"
            testId={props['data-testid']}
            className={twJoin('flex-shrink-0 flex-grow basis-0', className)}
            type="text"
            aria-label={getText('editNameShortcut')}
            // we don't want the display the default error message
            error={null}
            onContextMenu={(event) => {
              event.stopPropagation()
            }}
            onDoubleClick={(event) => {
              // Double click should select text, rather than triggering an action.
              event.stopPropagation()
            }}
            onKeyDown={(event) => {
              if (event.key === 'Escape') {
                event.preventDefault()
                onCancel()
              }
              event.stopPropagation()
            }}
          />

          {hasError && <ErrorMessage message={errorMessage} formRef={formRef} />}

          <div className="ml-1 flex w-auto flex-none basis-0 items-center gap-1.5">
            {form.formState.isDirty && (
              <Form.Submit
                size="medium"
                variant="icon"
                icon={TickIcon}
                aria-label={getText('confirmEdit')}
                children={null}
              />
            )}

            <Button
              size="medium"
              variant="icon"
              icon={CrossIcon}
              aria-label={getText('cancelEdit')}
              onPress={onCancel}
              children={null}
            />
          </div>
        </div>
      </Form.Provider>
    </form>
  )
}

/**
 * Props for {@link ErrorMessage}.
 */
interface ErrorMessageProps {
  readonly message: string
  readonly formRef: React.RefObject<HTMLFormElement>
}

/**
 * Error message for {@link EditableSpan}.
 */
function ErrorMessage(props: ErrorMessageProps) {
  const { message, formRef } = props

  const [measureFormRef, formRect] = useMeasure()

  const offset = 12
  const crossOffset = 30

  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  const outlineWidth = crossOffset + 10

  useLayoutEffect(() => {
    measureFormRef(formRef.current)
  }, [measureFormRef, formRef])

  if (formRect == null) {
    return null
  }

  return (
    <div
      className="pointer-events-none absolute"
      style={{
        width: formRect.width + outlineWidth,
        height: formRect.height + offset,
        transform: `translateX(-${crossOffset}px)`,
      }}
    >
      <div
        className="pointer-events-none absolute h-full w-full rounded-4xl border-[2px] border-danger"
        data-testid="error-message-outline"
      />

      <div data-testid="error-message-container" className="absolute bottom-0 right-0 top-0 z-1">
        <Underlay
          className="pointer-events-auto flex h-full max-w-[512px] items-center rounded-3xl rounded-l-none bg-danger pl-1.5 pr-2.5"
          style={{ transform: `translateX(100%)` }}
        >
          <Text testId="error-message-text" variant="body" truncate="1" color="invert">
            {message}
          </Text>

          <div className="absolute bottom-0 left-0 aspect-square w-5 -translate-x-full [background:radial-gradient(circle_at_0%_0%,_transparent_70%,_var(--color-danger)_70%)]" />
          <div className="absolute left-0 top-0 aspect-square w-5 -translate-x-full [background:radial-gradient(circle_at_0%_100%,_transparent_70%,_var(--color-danger)_70%)]" />
        </Underlay>
      </div>
    </div>
  )
}
