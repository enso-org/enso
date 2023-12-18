/** @file A Vue composable that calls one of two given callbacks, depending on whether a click is
 * a single click or a double click. */

/** Calls {@link onClick} if a click is a single click, or {@link onDoubleClick} if a click is
 * a double click. For this function, a double click is defined as a second click that occurs within
 * 200ms of the first click. The click count is reset to 0 upon double click, or after 200ms. */
export function useDoubleClick<Args extends any[]>(
  onClick: (...args: Args) => void,
  onDoubleClick: (...args: Args) => void,
) {
  const timeBetweenClicks = 200
  let clickCount = 0
  let singleClickTimer: ReturnType<typeof setTimeout>

  const handleClick = (...args: Args) => {
    clickCount++
    if (clickCount === 1) {
      onClick(...args)
      singleClickTimer = setTimeout(() => {
        clickCount = 0
      }, timeBetweenClicks)
    } else if (clickCount === 2) {
      clearTimeout(singleClickTimer)
      clickCount = 0
      onDoubleClick(...args)
    }
  }
  return { handleClick }
}
