/** @file A text `<span>` which turns into an `input` when desired. */
import * as React from 'react'

import CrossIcon from '#/assets/cross.svg'
import TickIcon from '#/assets/tick.svg'

import { Button, Form, Input, Text, Underlay } from '#/components/AriaComponents'
import * as textProvider from '#/providers/TextProvider'
import * as tailwindMerge from '#/utilities/tailwindMerge'

import { useInteractOutside } from '#/components/aria'
import { useAutoFocus } from '#/hooks/autoFocusHooks'
import { useMeasure } from '#/hooks/measureHooks'
import { AnimatePresence, motion, type Variants } from 'framer-motion'
import { useLayoutEffect } from 'react'
import type { z } from 'zod'

// eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unsafe-assignment
const MotionText = motion(Text)

/**
 * Props for {@link EditableSpan}.
 */
export interface EditableSpanProps {
  readonly 'data-testid'?: string
  readonly className?: string
  readonly editable?: boolean
  readonly onSubmit: (value: string) => Promise<void>
  readonly onCancel: () => void
  readonly children: string
  /**
   * Additional schema to validate the value.
   */
  readonly schema?: (schema: z.ZodType<string>) => z.ZodType<string>
}

/** A `<span>` that can turn into an `<input type="text">`. */
export default function EditableSpan(props: EditableSpanProps) {
  const { className = '', editable = false, children } = props

  return (
    <AnimatePresence initial={false}>
      {editable && <EditForm {...props} />}

      {!editable && (
        <MotionText
          className={tailwindMerge.twJoin('min-w-0', className)}
          testId={props['data-testid']}
          truncate="1"
          initial={{ opacity: 0, y: 5 }}
          animate={{ opacity: 1, y: 0 }}
          exit={{ opacity: 0, y: 5 }}
        >
          {children}
        </MotionText>
      )}
    </AnimatePresence>
  )
}

/**
 * Props for {@link EditForm}.
 */
interface EditFormProps extends EditableSpanProps {}

const CONTAINER_VARIANTS: Variants = {
  hidden: {
    opacity: 0,
    transition: {
      staggerChildren: 1,
    },
  },
  visible: {
    opacity: 1,
    transition: {
      staggerChildren: 1,
    },
  },
}

const CHILD_VARIANTS: Variants = {
  hidden: { opacity: 0, x: 5 },
  visible: { opacity: 1, x: 0 },
}

// eslint-disable-next-line @typescript-eslint/no-magic-numbers
const TRANSITION_OPTIONS = { stiffness: 300, damping: 150, mass: 1 }

/**
 * Edit form for {@link EditableSpan}.
 */
function EditForm(props: EditFormProps) {
  const { className = '', children, onSubmit, onCancel, schema } = props

  const { getText } = textProvider.useText()

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
            className={tailwindMerge.twJoin('flex-shrink-0 flex-grow basis-0', className)}
            type="text"
            aria-label={getText('editNameShortcut')}
            // we don't want the display the default error message
            error={null}
            onContextMenu={(event) => {
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

          <AnimatePresence>
            {hasError && <ErrorMessage message={errorMessage} formRef={formRef} />}
          </AnimatePresence>

          <AnimatePresence>
            <motion.div
              variants={CONTAINER_VARIANTS}
              initial="hidden"
              animate="visible"
              exit="hidden"
              className="ml-1 flex w-auto flex-none basis-0 items-center gap-1.5"
            >
              {form.formState.isDirty && (
                <motion.div
                  variants={CHILD_VARIANTS}
                  transition={TRANSITION_OPTIONS}
                  initial="hidden"
                  animate="visible"
                  exit="hidden"
                >
                  <Form.Submit
                    size="medium"
                    variant="icon"
                    icon={TickIcon}
                    aria-label={getText('confirmEdit')}
                    children={null}
                  />
                </motion.div>
              )}

              <motion.div
                variants={CHILD_VARIANTS}
                // eslint-disable-next-line @typescript-eslint/no-magic-numbers
                transition={{ ...TRANSITION_OPTIONS, delay: 0.25 }}
                initial="hidden"
                animate="visible"
                exit="hidden"
              >
                <Button
                  size="medium"
                  variant="icon"
                  icon={CrossIcon}
                  aria-label={getText('cancelEdit')}
                  onPress={onCancel}
                  children={null}
                />
              </motion.div>
            </motion.div>
          </AnimatePresence>
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

  const [measureFormRef, formRect] = useMeasure({ useRAF: false })

  const offset = 12
  const crossOffset = 36

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
      <motion.div
        layout
        // eslint-disable-next-line @typescript-eslint/no-magic-numbers
        initial={{ opacity: 0, scaleX: 0.99 }}
        animate={{ opacity: 1, scaleX: 1 }}
        // eslint-disable-next-line @typescript-eslint/no-magic-numbers
        exit={{ opacity: 0, scaleX: 0.99 }}
        className="pointer-events-none absolute h-full w-full rounded-4xl border-[2px] border-danger"
        data-testid="error-message-outline"
      />

      <motion.div
        // eslint-disable-next-line @typescript-eslint/no-magic-numbers
        initial={{ opacity: 0, x: -4 }}
        animate={{ opacity: 1, x: 0 }}
        // eslint-disable-next-line @typescript-eslint/no-magic-numbers
        exit={{ x: -4, opacity: 0 }}
        data-testid="error-message-container"
        className="absolute bottom-0 right-0 top-0 z-1"
      >
        <Underlay
          className="pointer-events-auto flex h-full max-w-[512px] items-center rounded-3xl rounded-l-none bg-danger pl-1.5 pr-2.5"
          style={{ transform: `translateX(100%)` }}
        >
          <MotionText
            // eslint-disable-next-line @typescript-eslint/no-magic-numbers
            initial={{ filter: 'blur(8px)', opacity: 0, x: -12 }}
            animate={{ filter: 'blur(0px)', opacity: 1, x: 0 }}
            // eslint-disable-next-line @typescript-eslint/no-magic-numbers
            exit={{ filter: 'blur(8px)', opacity: 0, x: -12 }}
            // eslint-disable-next-line @typescript-eslint/no-magic-numbers
            transition={{ stiffness: 550, damping: 150, mass: 4 }}
            testId="error-message-text"
            variant="body"
            truncate="1"
            color="invert"
          >
            {message}
          </MotionText>

          <div className="absolute bottom-0 left-0 aspect-square w-5 -translate-x-full [background:radial-gradient(circle_at_0%_0%,_transparent_70%,_var(--color-danger)_70%)]" />
          <div className="absolute left-0 top-0 aspect-square w-5 -translate-x-full [background:radial-gradient(circle_at_0%_100%,_transparent_70%,_var(--color-danger)_70%)]" />
        </Underlay>
      </motion.div>
    </div>
  )
}
