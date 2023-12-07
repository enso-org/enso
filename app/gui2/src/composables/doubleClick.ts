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
