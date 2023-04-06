/** @file */
/* eslint-disable @typescript-eslint/naming-convention */
import * as react from 'react'
import * as reactDom from 'react-dom'

// =============
// === Modal ===
// =============

interface ModalProps {
    visible: boolean
    onCancel?: () => void
}

function Modal(props: react.PropsWithChildren<ModalProps>) {
    const { children, visible, onCancel } = props

    /** Ensure that the container is only created once for each component. */
    const container = react.useRef(document.createElement('div')).current
    /** The div with this id is included in the `index.html`, so it can be asserted as non-empty. */
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const modalRoot = document.getElementById('modal-root')!

    react.useEffect(() => {
        document.body.classList.add('overflow-y-hidden', 'h-screen')
        modalRoot.appendChild(container)
        return () => {
            document.body.classList.remove('overflow-y-hidden', 'h-screen')
            modalRoot.removeChild(container)
        }
    }, [])

    const handleClickOver: React.MouseEventHandler<HTMLDivElement> = event => {
        if (event.currentTarget === event.target) {
            onCancel?.()
        }
    }

    return visible
        ? reactDom.createPortal(
              <div className="fixed top-0 bottom-0 right-0 left-0 overflow-hidden transform">
                  <div className="w-full h-full bg-black bg-opacity-30" onClick={handleClickOver} />
                  <div className="absolute top-1/2 right-1/2 transform translate-x-1/2 -translate-y-1/2">
                      {children}
                  </div>
              </div>,
              container
          )
        : null
}

export default Modal
