/**
 * @file A dialog is an overlay shown above other content in an application.
 * Can be used to display alerts, confirmations, or other content.
 */
import * as aria from '#/components/aria'
import { CloseButton, ResetButtonGroupContext } from '#/components/Button'
import { ErrorBoundary } from '#/components/ErrorBoundary'
import { usePortalContext } from '#/components/Portal'
import { Suspense } from '#/components/Suspense'
import { Text } from '#/components/Text'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useMeasure } from '#/hooks/measureHooks'
import { mergeRefs } from '#/utilities/mergeRefs'
import { LayoutGroup, motion, type Spring } from '#/utilities/motion'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { unsafeWriteValue } from '#/utilities/write'
import * as React from 'react'
import { useAppRoot } from '../UIProviders'
import { Close } from './Close'
import { DialogProvider } from './DialogProvider'
import { DialogStackRegistrar, type DialogStackItem } from './DialogStackProvider'
import { DialogTrigger } from './DialogTrigger'
import type * as types from './types'
import { animateScale, useInteractOutside } from './utilities'
import { DIALOG_MODAL_STYLES, DIALOG_OVERLAY_STYLES, DIALOG_STYLES } from './variants'

// eslint-disable-next-line no-restricted-syntax
const MotionDialog = motion(aria.Dialog)

const TRANSITION: Spring = {
  type: 'spring',
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  stiffness: 1_200,
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  damping: 90,
  mass: 3,
}

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

  const root = usePortalContext()

  return (
    <aria.ModalOverlay
      className={({ isEntering, isExiting }) =>
        DIALOG_OVERLAY_STYLES({ isEntering, isExiting, blockInteractions: !isDismissable })
      }
      ref={(element) => {
        if (element) {
          element.addEventListener('keydown', (event) => {
            if (event.key !== 'Escape') {
              event.stopPropagation()
            }
          })
        }
      }}
      isDismissable={isDismissable}
      isKeyboardDismissDisabled={isKeyboardDismissDisabled}
      UNSTABLE_portalContainer={root}
      onOpenChange={onOpenChange}
      shouldCloseOnInteractOutside={() => false}
      {...modalProps}
    >
      {(values) => (
        <aria.Modal
          className={({ isEntering, isExiting }) =>
            DIALOG_MODAL_STYLES({ type, isEntering, isExiting })
          }
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

const TYPE_TO_DIALOG_TYPE: Record<NonNullable<DialogProps['type']>, DialogStackItem['type']> = {
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
    testId,
    title,
    children,
    isDismissable = true,
    onDismiss,
    ...ariaDialogProps
  } = props

  const dialogRef = React.useRef<HTMLDivElement>(null)
  const scrollerRef = React.useRef<HTMLDivElement | null>(null)
  const dialogId = aria.useId()
  const appRoot = useAppRoot()

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

  // Mutating the method of the modalState object to ensure that the close `close`
  // function will call the `onDismiss` function and then close the modal.
  unsafeWriteValue(
    modalState,
    'close',
    useEventCallback(() => {
      onDismiss?.()
      modalState.setOpen(false)
    }),
  )

  const close = useEventCallback(() => {
    modalState.close()
  })

  useInteractOutside({
    ref: dialogRef,
    id: dialogId,
    onInteractOutside: () => {
      if (isDismissable) {
        close()
      } else {
        if (dialogRef.current) {
          // eslint-disable-next-line @typescript-eslint/no-magic-numbers
          animateScale(dialogRef.current, 1.02)
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
            mergeRefs(dialogRef, (element) => {
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
              close={close}
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
              close={close}
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
        </MotionDialog>

        <DialogStackRegistrar id={dialogId} type={TYPE_TO_DIALOG_TYPE[type]} />
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
        <ErrorBoundary>
          <Suspense loaderProps={{ minHeight: type === 'fullscreen' ? 'full' : 'h32' }}>
            <DialogProvider close={close} dialogId={dialogId}>
              {typeof children === 'function' ? children({ close }) : children}
            </DialogProvider>
          </Suspense>
        </ErrorBoundary>
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
      {closeButton !== 'none' && <CloseButton className={styles.closeButton()} onPress={close} />}

      {title != null && (
        <Text.Heading id={titleId} level={2} className={styles.heading()} weight="semibold">
          {title}
        </Text.Heading>
      )}
    </aria.Header>
  )
})

Dialog.Close = Close
Dialog.Trigger = DialogTrigger
