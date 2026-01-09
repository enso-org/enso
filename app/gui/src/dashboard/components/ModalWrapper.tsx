/** @file A wrapper component that provides the modal used in the dashboard. */
import { ErrorBoundary } from '#/components/ErrorBoundary'
import Portal from '#/components/Portal'
import TheModal from '#/pages/dashboard/components/TheModal'

/** A wrapper component that provides the modal used in the dashboard. */
export function ModalWrapper() {
  return (
    <ErrorBoundary>
      <Portal>
        <div className="select-none text-xs text-primary">
          <TheModal />
        </div>
      </Portal>
    </ErrorBoundary>
  )
}
