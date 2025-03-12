/** @file Components for `testUtils`. */
import { Form, type FormProps, type TSchema } from '#/components/AriaComponents'
import UIProviders from '#/components/UIProviders'
import { QueryClientProvider, type QueryClient } from '@tanstack/react-query'
import { createQueryClient } from 'enso-common/src/queryClient'
import { useState, type PropsWithChildren, type ReactNode } from 'react'

/** A wrapper that passes through its children. */
export function PassThroughWrapper({ children }: PropsWithChildren) {
  return children
}

/** A wrapper that provides the {@link UIProviders} context. */
export function UIProvidersWrapper({
  children,
}: {
  children?: ReactNode | ((props: { queryClient: QueryClient }) => ReactNode)
}) {
  const [queryClient] = useState(() => createQueryClient())

  return (
    <QueryClientProvider client={queryClient}>
      <UIProviders appRoot={document.body} portalRoot={document.body} locale="en">
        {typeof children === 'function' ? children({ queryClient }) : children}
      </UIProviders>
    </QueryClientProvider>
  )
}

/** A wrapper that provides the {@link Form} context. */
export function FormWrapper<Schema extends TSchema, SubmitResult = void>(
  props: FormProps<Schema, SubmitResult>,
) {
  return <Form {...props} />
}
