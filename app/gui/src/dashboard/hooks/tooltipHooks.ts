/** @file Hooks related to tooltips. */
import * as React from 'react'

// =======================
// === useNeedsTooltip ===
// =======================

/** Whether a given element needs a tooltip. */
export function useNeedsTooltip() {
  const [needsTooltip, setNeedsTooltip] = React.useState(false)
  const nameCellCleanupRef = React.useRef(() => {})
  const [resizeObserver] = React.useState(
    () =>
      new ResizeObserver((changes) => {
        for (const change of changes.slice(0, 1)) {
          if (change.target instanceof HTMLElement) {
            setNeedsTooltip(change.target.clientWidth < change.target.scrollWidth)
          }
        }
      }),
  )
  const tooltipTargetRef = (element: Element | null) => {
    nameCellCleanupRef.current()
    if (element == null) {
      nameCellCleanupRef.current = () => {}
    } else {
      setNeedsTooltip(element.clientWidth < element.scrollWidth)
      resizeObserver.observe(element)
      nameCellCleanupRef.current = () => {
        resizeObserver.unobserve(element)
      }
    }
  }
  return { needsTooltip, tooltipTargetRef }
}
