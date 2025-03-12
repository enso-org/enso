/**
 * @file Utility functions for testing.
 *
 * **IMPORTANT**: This file is supposed to be used instead of `@testing-library/react`
 * It is used to provide a portal root and locale to all tests.
 */
/// <reference types="@testing-library/jest-dom" />
import type { FormInstance, FormProps, TSchema } from '#/components/AriaComponents'
import { type QueryClient } from '@tanstack/react-query'
import {
  render,
  renderHook,
  type RenderHookOptions,
  type RenderHookResult,
  type RenderOptions,
  type RenderResult,
} from '@testing-library/react'
import { Fragment, createElement, type ReactElement } from 'react'
import { FormWrapper, PassThroughWrapper, UIProvidersWrapper } from './components'

/** Result type for {@link renderWithRoot}. */
interface RenderWithRootResult extends RenderResult {
  readonly queryClient: QueryClient
}

/** Custom render function for tests. */
function renderWithRoot(
  ui: ReactElement,
  options?: Omit<RenderOptions, 'queries'>,
): RenderWithRootResult {
  const { wrapper: Wrapper = PassThroughWrapper, ...rest } = options ?? {}

  let queryClient: QueryClient

  const result = render(ui, {
    wrapper: ({ children }) =>
      createElement(UIProvidersWrapper, {
        children: ({ queryClient: queryClientFromWrapper }) => {
          queryClient = queryClientFromWrapper
          return createElement(Wrapper, { children })
        },
      }),
    ...rest,
  })

  return {
    ...result,
    // @ts-expect-error - This is safe because we render before returning the result,
    // so the queryClient is guaranteed to be set.
    queryClient,
  } as const
}

/** Result type for {@link renderWithForm}. */
interface RenderWithFormResult<Schema extends TSchema> extends RenderWithRootResult {
  readonly form: FormInstance<Schema>
}

/** Add a form wrapper to the component. */
function renderWithForm<Schema extends TSchema, SubmitResult = void>(
  ui: ReactElement,
  options: Omit<RenderOptions, 'queries' | 'wrapper'> & {
    formProps: FormProps<Schema, SubmitResult>
  },
): RenderWithFormResult<Schema> {
  const { formProps, ...rest } = options

  let form: FormInstance<Schema>

  const result = renderWithRoot(ui, {
    wrapper: ({ children }) =>
      createElement<Parameters<typeof FormWrapper<Schema, SubmitResult>>[0]>(FormWrapper, {
        ...formProps,
        children: ({ form: formFromWrapper }) => {
          form = formFromWrapper
          return createElement(Fragment, { children })
        },
      }),
    ...rest,
  })

  return {
    ...result,
    // @ts-expect-error - This is safe because we render before returning the result,
    // so the form is guaranteed to be set.
    form,
  } as const
}

/** Result type for {@link renderHookWithRoot}. */
interface RenderHookWithRootResult<Result, Props> extends RenderHookResult<Result, Props> {
  readonly queryClient: QueryClient
}

/** A custom renderHook function for tests. */
function renderHookWithRoot<Result, Props>(
  hook: (props: Props) => Result,
  options?: Omit<RenderHookOptions<Props>, 'queries'>,
): RenderHookWithRootResult<Result, Props> {
  let queryClient: QueryClient

  const result = renderHook(hook, {
    wrapper: ({ children }) =>
      createElement(UIProvidersWrapper, {
        children: ({ queryClient: queryClientFromWrapper }) => {
          queryClient = queryClientFromWrapper
          return createElement(Fragment, { children })
        },
      }),
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
    wrapper: ({ children }) =>
      createElement<Parameters<typeof FormWrapper<Schema, SubmitResult>>[0]>(FormWrapper, {
        ...formProps,
        children: ({ form: formFromWrapper }) => {
          form = formFromWrapper
          return createElement(Fragment, { children })
        },
      }),
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
