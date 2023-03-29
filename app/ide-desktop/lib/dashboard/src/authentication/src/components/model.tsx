/** @file */
/* eslint-disable @typescript-eslint/naming-convention */
import { PropsWithChildren, useEffect, useRef } from 'react'
import { createPortal } from 'react-dom'

interface ModalImpProps {
    onCancel?: () => void
}
function ModalImp(props: PropsWithChildren<ModalImpProps>) {
    const { children, onCancel } = props
    /** Ensure that the container is only created once for each component. */
    const containerRef = useRef(document.createElement('div'))
    const container = containerRef.current
    /** The div with this id is included in the `index.html`, so it can be asserted as non-empty. */
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const modalRoot = document.getElementById('modal-root')!

    useEffect(() => {
        document.body.classList.add('overflow-y-hidden', 'h-screen')
        modalRoot.appendChild(container)
        return () => {
            document.body.classList.remove('overflow-y-hidden', 'h-screen')
            modalRoot.removeChild(container)
        }
    }, [])

    const handleClickOver: React.MouseEventHandler<HTMLDivElement> = ev => {
        if (ev.currentTarget !== ev.target) return
        onCancel?.()
    }

    return createPortal(
        <div className="fixed top-0 bottom-0 right-0 left-0 overflow-hidden transform">
            <div className="w-full h-full bg-black bg-opacity-30" onClick={handleClickOver} />
            <div className="absolute top-1/2 right-1/2 transform translate-x-1/2 -translate-y-1/2">
                {children}
            </div>
        </div>,
        containerRef.current
    )
}
interface ModalProps {
    visible: boolean
    onCancel?: () => void
}
function Modal(props: PropsWithChildren<ModalProps>) {
    const { children, visible, onCancel = () => {} } = props

    return visible ? <ModalImp onCancel={onCancel}>{children}</ModalImp> : null
}

export default Modal
