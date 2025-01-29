/**
 * @file A dialog is an overlay shown above other content in an application.
 * Can be used to display alerts, confirmations, or other content.
 */
import * as React from 'react'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import * as errorBoundary from '#/components/ErrorBoundary'
import * as portal from '#/components/Portal'
import * as suspense from '#/components/Suspense'

import * as mergeRefs from '#/utilities/mergeRefs'

import { DialogDismiss, ResetButtonGroupContext } from '#/components/AriaComponents'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useMeasure } from '#/hooks/measureHooks'
import { LayoutGroup, motion, type Spring } from '#/utilities/motion'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { tv } from '#/utilities/tailwindVariants'
import { unsafeWriteValue } from '#/utilities/write'
import { useRootContext } from '../../UIProviders'
import { Close } from './Close'
import * as dialogProvider from './DialogProvider'
import * as dialogStackProvider from './DialogStackProvider'
import { DialogTrigger } from './DialogTrigger'
import type * as types from './types'
import * as utlities from './utilities'
import { DIALOG_BACKGROUND } from './variants'

// eslint-disable-next-line no-restricted-syntax
const MotionDialog = motion(aria.Dialog)

const OVERLAY_STYLES = tv({
  base: 'fixed inset-0 isolate flex items-center justify-center bg-primary/20',
  variants: {
    isEntering: { true: 'animate-in fade-in duration-200 ease-out' },
    isExiting: { true: 'animate-out fade-out duration-200 ease-in' },
    blockInteractions: { true: 'backdrop-blur-md transition-[backdrop-filter] duration-200' },
  },
})

const MODAL_STYLES = tv({
  base: 'fixed inset-0 flex items-center justify-center text-xs text-primary',
  variants: {
    isEntering: { true: 'animate-in ease-out duration-200' },
    isExiting: { true: 'animate-out ease-in duration-200' },
    type: { modal: '', fullscreen: 'p-3.5' },
  },
  compoundVariants: [
    { type: 'modal', isEntering: true, class: 'slide-in-from-top-1' },
    { type: 'modal', isExiting: true, class: 'slide-out-to-top-1' },
    { type: 'fullscreen', isEntering: true, class: 'zoom-in-[1.015]' },
    { type: 'fullscreen', isExiting: true, class: 'zoom-out-[1.015]' },
  ],
})

const DIALOG_STYLES = tv({
  base: DIALOG_BACKGROUND({
    className: 'w-full max-w-full flex flex-col text-left align-middle shadow-xl overflow-clip',
  }),
  variants: {
    type: {
      modal: {
        base: 'w-full min-h-[100px] max-h-[90vh]',
        header: 'px-3.5 pt-[3px] pb-0.5 min-h-[42px]',
      },
      fullscreen: {
        base: 'w-full h-full max-w-full max-h-full bg-clip-border',
        header: 'px-4 pt-[5px] pb-1.5 min-h-12',
      },
    },
    fitContent: {
      true: {
        base: 'min-w-max',
        content: 'min-w-max',
      },
    },
    hideCloseButton: { true: { closeButton: 'hidden' } },
    closeButton: {
      normal: { base: '', closeButton: '' },
      floating: {
        base: '',
        closeButton: 'absolute left-4 top-4 visible z-1 transition-all duration-150',
        header: 'p-0 max-h-0 min-h-0 h-0 border-0 z-1',
        content: 'isolate',
      },
      none: {},
    },
    rounded: {
      none: { base: '' },
      small: { base: 'rounded-sm' },
      medium: { base: 'rounded-md' },
      large: { base: 'rounded-lg' },
      xlarge: { base: 'rounded-xl' },
      xxlarge: { base: 'rounded-2xl', scroller: 'scroll-offset-edge-2xl' },
      xxxlarge: { base: 'rounded-3xl', scroller: 'scroll-offset-edge-3xl' },
      xxxxlarge: { base: 'rounded-4xl', scroller: 'scroll-offset-edge-4xl' },
    },
    /**
     * The size of the dialog.
     * Only applies to the `modal` type.
     */
    size: {
      small: { base: '' },
      medium: { base: '' },
      large: { base: '' },
      xlarge: { base: '' },
      xxlarge: { base: '' },
      xxxlarge: { base: '' },
      xxxxlarge: { base: '' },
    },
    padding: {
      none: { content: 'p-0' },
      small: { content: 'px-1 pt-3.5 pb-3.5' },
      medium: { content: 'px-4 pt-3 pb-4' },
      large: { content: 'px-8 pt-5 pb-5' },
      xlarge: { content: 'p-12 pt-6 pb-8' },
      xxlarge: { content: 'p-16 pt-8 pb-12' },
      xxxlarge: { content: 'p-20 pt-10 pb-16' },
    },
    scrolledToTop: { true: { header: 'border-transparent' } },
    layout: { true: { measurerWrapper: 'h-auto' }, false: { measurerWrapper: 'h-full' } },
  },
  slots: {
    header:
      'sticky z-1 top-0 grid grid-cols-[1fr_auto_1fr] items-center border-b border-primary/10 transition-[border-color] duration-150',
    closeButton: 'col-start-1 col-end-1 mr-auto',
    heading: 'col-start-2 col-end-2 my-0 text-center',
    scroller: 'flex flex-col h-full overflow-y-auto max-h-[inherit]',
    measurerWrapper: 'inline-grid min-h-fit w-full grid-rows-1',
    content: 'inline-block max-h-fit min-h-fit [grid-area:1/1] min-w-0',
  },
  compoundVariants: [
    { type: 'modal', size: 'small', class: 'max-w-sm' },
    { type: 'modal', size: 'medium', class: 'max-w-md' },
    { type: 'modal', size: 'large', class: 'max-w-lg' },
    { type: 'modal', size: 'xlarge', class: 'max-w-xl' },
    { type: 'modal', size: 'xxlarge', class: 'max-w-2xl' },
    { type: 'modal', size: 'xxxlarge', class: 'max-w-3xl' },
    { type: 'modal', size: 'xxxxlarge', class: 'max-w-4xl' },
    { type: 'fullscreen', class: { measurerWrapper: 'h-full' } },
  ],
  defaultVariants: {
    layout: true,
    type: 'modal',
    closeButton: 'normal',
    hideCloseButton: false,
    size: 'medium',
    padding: 'none',
    rounded: 'xxxlarge',
  },
})

const TRANSITION: Spring = {
  type: 'spring',
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  stiffness: 1_200,
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  damping: 90,
  mass: 3,
}

// ==============
// === Dialog ===
// ==============

/** Props for the {@link Dialog} component. */
export interface DialogProps
  extends types.DialogProps,
    Omit<VariantProps<typeof DIALOG_STYLES>, 'scrolledToTop'> {}

/**
 * A dialog is an overlay shown above other content in an application.
 * Can be used to display alerts, confirmations, or other content.
 */
export function Dialog(props: DialogProps) {
  const {
    type = 'modal',
    isDismissable = true,
    isKeyboardDismissDisabled = false,
    onOpenChange = () => {},
    modalProps = {},
  } = props

  const root = portal.useStrictPortalContext()

  return (
    <aria.ModalOverlay
      className={({ isEntering, isExiting }) =>
        OVERLAY_STYLES({ isEntering, isExiting, blockInteractions: !isDismissable })
      }
      isDismissable={isDismissable}
      isKeyboardDismissDisabled={isKeyboardDismissDisabled}
      UNSTABLE_portalContainer={root}
      onOpenChange={onOpenChange}
      shouldCloseOnInteractOutside={() => false}
      {...modalProps}
    >
      {(values) => (
        <aria.Modal
          className={({ isEntering, isExiting }) => MODAL_STYLES({ type, isEntering, isExiting })}
          isDismissable={isDismissable}
          isKeyboardDismissDisabled={isKeyboardDismissDisabled}
          UNSTABLE_portalContainer={root}
          onOpenChange={onOpenChange}
          shouldCloseOnInteractOutside={() => false}
          {...modalProps}
        >
          <DialogContent {...props} modalState={values.state} />
        </aria.Modal>
      )}
    </aria.ModalOverlay>
  )
}

const TYPE_TO_DIALOG_TYPE: Record<
  NonNullable<DialogProps['type']>,
  dialogStackProvider.DialogStackItem['type']
> = {
  modal: 'dialog',
  fullscreen: 'dialog-fullscreen',
}

/**
 * Props for the {@link DialogContent} component.
 */
interface DialogContentProps extends DialogProps, VariantProps<typeof DIALOG_STYLES> {
  readonly modalState: aria.OverlayTriggerState
}

/**
 * The content of a dialog.
 * @internal
 */
function DialogContent(props: DialogContentProps) {
  const {
    variants = DIALOG_STYLES,
    modalState,
    className,
    type = 'modal',
    rounded,
    hideCloseButton = false,
    closeButton = 'normal',
    size,
    padding: paddingRaw,
    fitContent,
    layout,
    testId = 'dialog',
    title,
    children,
    isDismissable = true,
    ...ariaDialogProps
  } = props

  const dialogRef = React.useRef<HTMLDivElement>(null)
  const scrollerRef = React.useRef<HTMLDivElement | null>(null)
  const dialogId = aria.useId()

  const { appRoot } = useRootContext()

  const titleId = `${dialogId}-title`
  const padding = paddingRaw ?? (type === 'modal' ? 'medium' : 'xlarge')
  const isFullscreen = type === 'fullscreen'

  const [isLayoutDisabled, setIsLayoutDisabled] = React.useState(true)

  const [contentDimensionsRef, dimensions] = useMeasure({
    isDisabled: isLayoutDisabled,
    useRAF: false,
  })

  const [headerDimensionsRef, headerDimensions] = useMeasure({
    isDisabled: isLayoutDisabled,
    useRAF: false,
  })

  utlities.useInteractOutside({
    ref: dialogRef,
    id: dialogId,
    onInteractOutside: () => {
      if (isDismissable) {
        modalState.close()
      } else {
        if (dialogRef.current) {
          // eslint-disable-next-line @typescript-eslint/no-magic-numbers
          utlities.animateScale(dialogRef.current, 1.02)
        }
      }
    },
  })

  React.useEffect(() => {
    if (isFullscreen) {
      return
    }

    setIsLayoutDisabled(false)

    return () => {
      setIsLayoutDisabled(true)
    }
  }, [isFullscreen])

  React.useEffect(() => {
    if (isFullscreen && modalState.isOpen) {
      unsafeWriteValue(appRoot.style, 'scale', '0.99')
      unsafeWriteValue(appRoot.style, 'filter', 'blur(8px)')
      unsafeWriteValue(appRoot.style, 'willChange', 'scale, filter')

      return () => {
        unsafeWriteValue(appRoot.style, 'scale', '')
        unsafeWriteValue(appRoot.style, 'filter', '')
        unsafeWriteValue(appRoot.style, 'willChange', '')
      }
    }
  }, [isFullscreen, modalState, appRoot])

  const styles = variants({
    className,
    type,
    rounded,
    hideCloseButton,
    closeButton,
    size,
    padding,
    fitContent,
    layout,
  })

  const getDialogHeight = () => {
    if (isFullscreen) {
      return ''
    }

    if (dimensions == null || headerDimensions == null) {
      return ''
    }

    return dimensions.height + headerDimensions.height
  }

  return (
    <ResetButtonGroupContext>
      <LayoutGroup>
        <MotionDialog
          layout
          transition={TRANSITION}
          style={{ height: getDialogHeight() }}
          id={dialogId}
          onLayoutAnimationStart={() => {
            if (scrollerRef.current) {
              scrollerRef.current.style.overflowY = 'clip'
            }
          }}
          onLayoutAnimationComplete={() => {
            if (scrollerRef.current) {
              scrollerRef.current.style.overflowY = ''
            }
          }}
          ref={(ref: HTMLDivElement | null) => {
            mergeRefs.mergeRefs(dialogRef, (element) => {
              if (element) {
                // This is a workaround for the `data-testid` attribute not being
                // supported by the 'react-aria-components' library.
                // We need to set the `data-testid` attribute on the dialog element
                // so that we can use it in our tests.
                // This is a temporary solution until we refactor the Dialog component
                // to use `useDialog` hook from the 'react-aria-components' library.
                // this will allow us to set the `data-testid` attribute on the dialog
                element.dataset.testid = testId
              }
            })(ref)
          }}
          className={styles.base()}
          aria-labelledby={titleId}
          {...ariaDialogProps}
        >
          {(opts) => (
            <>
              <motion.div layout className="w-full" transition={{ duration: 0 }}>
                <DialogHeader
                  closeButton={closeButton}
                  title={title}
                  titleId={titleId}
                  scrollerRef={scrollerRef}
                  fitContent={fitContent}
                  hideCloseButton={hideCloseButton}
                  padding={padding}
                  rounded={rounded}
                  size={size}
                  type={type}
                  headerDimensionsRef={headerDimensionsRef}
                  close={opts.close}
                  variants={variants}
                />
              </motion.div>

              <motion.div
                layout
                layoutScroll
                className={styles.scroller()}
                ref={scrollerRef}
                transition={{ duration: 0 }}
              >
                <DialogBody
                  close={opts.close}
                  contentDimensionsRef={contentDimensionsRef}
                  dialogId={dialogId}
                  headerDimensionsRef={headerDimensionsRef}
                  scrollerRef={scrollerRef}
                  measurerWrapperClassName={styles.measurerWrapper()}
                  contentClassName={styles.content()}
                  type={type}
                >
                  {children}
                </DialogBody>
              </motion.div>
            </>
          )}
        </MotionDialog>

        <dialogStackProvider.DialogStackRegistrar id={dialogId} type={TYPE_TO_DIALOG_TYPE[type]} />
      </LayoutGroup>
    </ResetButtonGroupContext>
  )
}

/**
 * Props for the {@link DialogBody} component.
 */
interface DialogBodyProps {
  readonly dialogId: string
  readonly contentDimensionsRef: (node: HTMLElement | null) => void
  readonly headerDimensionsRef: (node: HTMLElement | null) => void
  readonly scrollerRef: React.RefObject<HTMLDivElement>
  readonly close: () => void
  readonly measurerWrapperClassName: string
  readonly contentClassName: string
  readonly children: DialogProps['children']
  readonly type: DialogProps['type']
}

/**
 * The internals of a dialog. Exists only as a performance optimization.
 */
// eslint-disable-next-line no-restricted-syntax
const DialogBody = React.memo(function DialogBody(props: DialogBodyProps) {
  const {
    close,
    contentDimensionsRef,
    dialogId,
    children,
    measurerWrapperClassName,
    contentClassName,
    type,
  } = props

  return (
    <div className={measurerWrapperClassName}>
      <div ref={contentDimensionsRef} className={contentClassName}>
        <errorBoundary.ErrorBoundary>
          <suspense.Suspense loaderProps={{ minHeight: type === 'fullscreen' ? 'full' : 'h32' }}>
            <dialogProvider.DialogProvider close={close} dialogId={dialogId}>
              {typeof children === 'function' ? children({ close }) : children}
            </dialogProvider.DialogProvider>
          </suspense.Suspense>
        </errorBoundary.ErrorBoundary>
      </div>
    </div>
  )
})

/**
 * Props for the {@link DialogHeader} component.
 */
interface DialogHeaderProps extends Omit<VariantProps<typeof DIALOG_STYLES>, 'scrolledToTop'> {
  readonly closeButton: DialogProps['closeButton']
  readonly title: DialogProps['title']
  readonly titleId: string
  readonly headerDimensionsRef: (node: HTMLElement | null) => void
  readonly scrollerRef: React.RefObject<HTMLDivElement>
  readonly close: () => void
}

/**
 * The header of a dialog.
 * @internal
 */
const DialogHeader = React.memo(function DialogHeader(props: DialogHeaderProps) {
  const {
    closeButton,
    title,
    titleId,
    headerDimensionsRef,
    scrollerRef,
    fitContent,
    hideCloseButton,
    padding,
    rounded,
    size,
    type,
    variants = DIALOG_STYLES,
    close,
    layout,
  } = props

  const styles = variants({
    type,
    closeButton,
    fitContent,
    hideCloseButton,
    padding,
    rounded,
    size,
    layout,
  })

  const [isScrolledToTop, privateSetIsScrolledToTop] = React.useState(true)

  const setIsScrolledToTop = React.useCallback(
    (value: boolean) => {
      React.startTransition(() => {
        privateSetIsScrolledToTop(value)
      })
    },
    [privateSetIsScrolledToTop],
  )

  /** Handles the scroll event on the dialog content. */
  const handleScrollEvent = useEventCallback(() => {
    if (scrollerRef.current) {
      setIsScrolledToTop(scrollerRef.current.scrollTop === 0)
    } else {
      setIsScrolledToTop(true)
    }
  })

  React.useEffect(() => {
    const scroller = scrollerRef.current
    if (scroller) {
      handleScrollEvent()

      scroller.addEventListener('scroll', handleScrollEvent, { passive: true })

      return () => {
        scroller.removeEventListener('scroll', handleScrollEvent)
      }
    }
  }, [handleScrollEvent, scrollerRef])

  return (
    <aria.Header
      ref={headerDimensionsRef}
      className={styles.header({ scrolledToTop: isScrolledToTop })}
    >
      {closeButton !== 'none' && (
        <ariaComponents.CloseButton className={styles.closeButton()} onPress={close} />
      )}

      {title != null && (
        <ariaComponents.Text.Heading
          id={titleId}
          level={2}
          className={styles.heading()}
          weight="semibold"
        >
          {title}
        </ariaComponents.Text.Heading>
      )}
    </aria.Header>
  )
})

Dialog.Close = Close
Dialog.Dismiss = DialogDismiss
Dialog.Trigger = DialogTrigger
