/**
 * @file
 *
 * Form error component.
 */
import Offline from '#/assets/offline_filled.svg'
import { Alert, Text, type AlertProps } from '#/components/AriaComponents'
import { useFormError, type UseFormErrorProps } from './useFormError'

/** Props for the FormError component. */
export interface FormErrorProps extends Omit<AlertProps, 'children'>, UseFormErrorProps {}

/** Form error component. */
export function FormError(props: FormErrorProps) {
  const { size = 'large', variant = 'error', rounded = 'xxlarge', ...alertProps } = props

  const errors = useFormError(props)

  if (errors.length === 0) {
    return null
  }

  return (
    <div className="flex w-full flex-col gap-4">
      {errors.map((error) => {
        const testId = `form-submit-${error.type}`
        const finalVariant = error.type === 'offline' ? 'outline' : variant
        const icon = error.type === 'offline' ? Offline : null

        return (
          <Alert
            key={error.message}
            size={size}
            variant={finalVariant}
            rounded={rounded}
            icon={icon}
            {...alertProps}
          >
            <Text variant="body" truncate="3" color="primary" testId={testId}>
              {error.message}
            </Text>
          </Alert>
        )
      })}
    </div>
  )
}
