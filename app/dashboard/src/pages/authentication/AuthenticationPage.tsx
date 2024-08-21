/** @file A styled authentication page.
 * This is a component, NOT a page, but it is here because it is related to the authentication pages
 * and nothing else. */
import type { ReactNode } from 'react'

import {
  DIALOG_BACKGROUND,
  type FieldValues,
  Form,
  type FormProps,
  type TSchema,
  Text,
} from '#/components/AriaComponents'
import Page from '#/components/Page'
import { useOffline } from '#/hooks/offlineHooks'
import { useText } from '#/providers/TextProvider'
import invariant from 'tiny-invariant'

// ==========================
// === AuthenticationPage ===
// ==========================

/** Props for an {@link AuthenticationPage}. */
interface AuthenticationPagePropsBase {
  readonly supportsOffline?: boolean
  readonly 'data-testid'?: string
  readonly title: string
  readonly footer?: ReactNode
}

/** Props for an {@link AuthenticationPage}. */
export type AuthenticationPageProps<Schema extends TSchema> = AuthenticationPagePropsBase &
  Partial<FormProps<Schema>>

/** A styled authentication page. */
export default function AuthenticationPage<Schema extends TSchema>(
  props: AuthenticationPageProps<Schema>,
) {
  const { title, children, footer, supportsOffline = false, ...formProps } = props
  const { form, schema, onSubmit } = formProps
  const isForm = onSubmit != null && (form != null || schema != null)

  const { getText } = useText()
  const { isOffline } = useOffline()

  const heading = (
    <Text.Heading level={1} className="self-center" weight="medium">
      {title}
    </Text.Heading>
  )

  const containerClasses = DIALOG_BACKGROUND({
    className: 'flex w-full flex-col gap-4 rounded-4xl p-12',
  })

  const offlineAlertClasses = DIALOG_BACKGROUND({
    className: 'flex mt-auto rounded-sm items-center justify-center p-4 px-12 rounded-4xl',
  })

  return (
    <Page>
      <div className="flex h-full w-full flex-col overflow-y-auto p-12">
        <div
          className="relative m-auto grid h-full w-full max-w-md grid-cols-1 grid-rows-[1fr_auto_1fr] flex-col items-center justify-center gap-auth text-sm text-primary"
          data-testid={props['data-testid']}
        >
          {isOffline && (
            <div className={offlineAlertClasses}>
              <Text className="text-center" balance elementType="p">
                {getText('loginUnavailableOffline')}{' '}
                {supportsOffline && getText('loginUnavailableOfflineLocal')}
              </Text>
            </div>
          )}

          <div className="row-start-2 row-end-3 flex w-full flex-col items-center gap-auth">
            {!isForm ?
              <div className={containerClasses}>
                {heading}
                {(() => {
                  invariant(
                    typeof children !== 'function',
                    'Non-forms should not have a function as a child.',
                  )
                  return children
                })()}
              </div>
            : <Form
                // This is SAFE, as the props type of this type extends `FormProps`.
                // eslint-disable-next-line no-restricted-syntax
                {...(formProps as FormProps<Schema>)}
                className={containerClasses}
              >
                {(innerProps) => (
                  <>
                    {heading}
                    {typeof children === 'function' ? children(innerProps) : children}
                  </>
                )}
              </Form>
            }
            {footer}
          </div>
        </div>
      </div>
    </Page>
  )
}
