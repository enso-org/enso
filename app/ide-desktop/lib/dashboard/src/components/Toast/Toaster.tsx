/**
 * @file
 *
 * A toaster component that displays toast notifications.
 */
import * as React from 'react'

import * as toast from 'sonner'

import * as ariaComponents from '#/components/AriaComponents'
import * as loader from '#/components/Loader'

/**
 * A toaster component that displays toast notifications.
 */
// eslint-disable-next-line no-restricted-syntax
export const Toaster = React.memo(function Toaster() {
  return (
    <toast.Toaster
      position="bottom-right"
      pauseWhenPageIsHidden
      icons={{
        loading: <loader.Loader minHeight="h4" size={16} appearDelayed={false} />,
        error: (
          <div
            className={ariaComponents.TEXT_STYLE({
              color: 'danger',
              variant: 'subtitle',
              disableLineHeightCompensation: true,
              textSelection: 'none',
              weight: 'medium',
              className:
                'fix-dark flex h-4 w-4 flex-none items-center justify-center rounded-full border border-current',
            })}
            /* eslint-disable-next-line no-restricted-syntax */
          >
            !
          </div>
        ),
      }}
      gap={8}
      toastOptions={{
        closeButton: true,
        unstyled: true,
        classNames: {
          toast: ariaComponents.DIALOG_BACKGROUND({
            variant: 'light',
            className:
              'group grid grid-cols-[auto_minmax(0,1fr)] place-items-baseline align-center px-4 py-3 rounded-2xl shadow-md gap-x-1 gap-y-1 min-w-[350px] max-w-[350px] select-none hacky-dark',
          }),
          icon: 'row-start-1 w-4 h-4 m-0 -mt-0.5 mr-1 self-start justify-center mt-[3px]',
          title: ariaComponents.TEXT_STYLE({
            color: 'primary',
            variant: 'subtitle',
            truncate: '1',
          }),
          description: ariaComponents.TEXT_STYLE({
            color: 'primary',
            variant: 'body',
            truncate: '3',
            disableLineHeightCompensation: true,
            className: '-mt-1 col-start-1 col-end-3',
          }),
          content: 'w-full row-start-1 col-start-2 col-end-5',
          closeButton: ariaComponents
            .BUTTON_STYLES({
              variant: 'custom',
              size: 'medium',
              rounded: 'full',
              iconOnly: true,
            })
            .base({
              className:
                'w-4.5 h-4.5 items-center justify-center -left-1.5 -top-1.5 transform-none shadow-sm text-primary/60 bg-white border-none opacity-0 group-hover:opacity-1 [&>svg]:w-3 [&>svg]:h-3 hover:text-primary/60 hover:shadow-xl hover:bg-white',
            }),
          actionButton: ariaComponents.BUTTON_STYLES({ variant: 'primary', size: 'xxsmall' }).base({
            className:
              'relative z-1 mt-auto row-start-1 col-start-4 col-end-5 shadow-dim-background group-hover:before:opacity-1',
          }),
          cancelButton: ariaComponents.BUTTON_STYLES({ variant: 'ghost', size: 'xxsmall' }).base({
            className:
              'relative z-1 mt-auto row-start-1 col-start-3 col-end-4 mr-1 bg-white shadow-dim-background group-hover:before:opacity-1',
          }),
          error: 'text-danger',
          info: 'text-primary',
          warning: 'text-primary',
          success: 'text-primary',
        },
      }}
      visibleToasts={6}
      offset={32}
      duration={5000}
    />
  )
})
