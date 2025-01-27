/**
 * @file Utility functions for testing.
 *
 * **IMPORTANT**: This file is supposed to be used instead of `@testing-library/react`
 * It is used to provide a portal root and locale to all tests.
 */
/// <reference types="@testing-library/jest-dom" />

import { Form, type FormInstance, type FormProps, type TSchema } from '#/components/AriaComponents'
import UIProviders from '#/components/UIProviders'
import { QueryClientProvider, type QueryClient } from '@tanstack/react-query'
import {
  render,
  renderHook,
  type RenderHookOptions,
  type RenderHookResult,
  type RenderOptions,
  type RenderResult,
} from '@testing-library/react'
import { createQueryClient } from 'enso-common/src/queryClient'
import { useState, type PropsWithChildren, type ReactElement, type ReactNode } from 'react'

/**
 * A wrapper that passes through its children.
 */
function PassThroughWrapper({ children }: PropsWithChildren) {
  return children
}

/**
 * A wrapper that provides the {@link UIProviders} context.
 */
function UIProvidersWrapper({
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

/**
 * A wrapper that provides the {@link Form} context.
 */
function FormWrapper<Schema extends TSchema, SubmitResult = void>(
  props: FormProps<Schema, SubmitResult>,
) {
  return <Form {...props} />
}

/**
 * Result type for {@link renderWithRoot}.
 */
interface RenderWithRootResult extends RenderResult {
  readonly queryClient: QueryClient
}

/**
 * Custom render function for tests.
 */
function renderWithRoot(
  ui: ReactElement,
  options?: Omit<RenderOptions, 'queries'>,
): RenderWithRootResult {
  const { wrapper: Wrapper = PassThroughWrapper, ...rest } = options ?? {}

  let queryClient: QueryClient

  const result = render(ui, {
    wrapper: ({ children }) => (
      <UIProvidersWrapper>
        {({ queryClient: queryClientFromWrapper }) => {
          queryClient = queryClientFromWrapper
          return <Wrapper>{children}</Wrapper>
        }}
      </UIProvidersWrapper>
    ),
    ...rest,
  })

  return {
    ...result,
    // @ts-expect-error - This is safe because we render before returning the result,
    // so the queryClient is guaranteed to be set.
    queryClient,
  } as const
}

/**
 * Result type for {@link renderWithForm}.
 */
interface RenderWithFormResult<Schema extends TSchema> extends RenderWithRootResult {
  readonly form: FormInstance<Schema>
}

/**
 * Adds a form wrapper to the component.
 */
function renderWithForm<Schema extends TSchema, SubmitResult = void>(
  ui: ReactElement,
  options: Omit<RenderOptions, 'queries' | 'wrapper'> & {
    formProps: FormProps<Schema, SubmitResult>
  },
): RenderWithFormResult<Schema> {
  const { formProps, ...rest } = options

  let form: FormInstance<Schema>

  const result = renderWithRoot(ui, {
    wrapper: ({ children }) => (
      <FormWrapper {...formProps}>
        {({ form: formFromWrapper }) => {
          form = formFromWrapper
          return <>{children}</>
        }}
      </FormWrapper>
    ),
    ...rest,
  })

  return {
    ...result,
    // @ts-expect-error - This is safe because we render before returning the result,
    // so the form is guaranteed to be set.
    form,
  } as const
}

/**
 * Result type for {@link renderHookWithRoot}.
 */
interface RenderHookWithRootResult<Result, Props> extends RenderHookResult<Result, Props> {
  readonly queryClient: QueryClient
}

/**
 * A custom renderHook function for tests.
 */
function renderHookWithRoot<Result, Props>(
  hook: (props: Props) => Result,
  options?: Omit<RenderHookOptions<Props>, 'queries'>,
): RenderHookWithRootResult<Result, Props> {
  let queryClient: QueryClient

  const result = renderHook(hook, {
    wrapper: ({ children }) => (
      <UIProvidersWrapper>
        {({ queryClient: queryClientFromWrapper }) => {
          queryClient = queryClientFromWrapper
          return <>{children}</>
        }}
      </UIProvidersWrapper>
    ),
    ...options,
  })

  return {
    ...result,
    // @ts-expect-error - This is safe because we render before returning the result,
    // so the queryClient is guaranteed to be set.
    queryClient,
  } as const
}

/**
 * Result type for {@link renderHookWithForm}.
 */
interface RenderHookWithFormResult<Result, Props, Schema extends TSchema>
  extends RenderHookWithRootResult<Result, Props> {
  readonly form: FormInstance<Schema>
}

/**
 * A custom renderHook function for tests that provides the {@link Form} context.
 */
function renderHookWithForm<Result, Props, Schema extends TSchema, SubmitResult = void>(
  hook: (props: Props) => Result,
  options: Omit<RenderHookOptions<Props>, 'queries' | 'wrapper'> & {
    formProps: FormProps<Schema, SubmitResult>
  },
): RenderHookWithFormResult<Result, Props, Schema> {
  const { formProps, ...rest } = options

  let form: FormInstance<Schema>
  const result = renderHookWithRoot(hook, {
    wrapper: ({ children }) => (
      <FormWrapper {...formProps}>
        {({ form: formFromWrapper }) => {
          form = formFromWrapper
          return <>{children}</>
        }}
      </FormWrapper>
    ),
    ...rest,
  })

  return {
    ...result,
    // @ts-expect-error - This is safe because we render before returning the result,
    // so the form is guaranteed to be set.
    form,
  } as const
}

export * from '@testing-library/react'
export { default as userEvent } from '@testing-library/user-event'
// override render method
export {
  renderWithRoot as render,
  renderHookWithRoot as renderHook,
  renderHookWithForm,
  renderWithForm,
}
