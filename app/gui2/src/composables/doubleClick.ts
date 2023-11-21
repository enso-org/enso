export function useDoubleClick(onClick: Function, onDoubleClick: Function) {
  const timeBetweenClicks = 200
  let lastClickTime = 0
  let clickCount = 0
  let singleClickTimer: ReturnType<typeof setTimeout>

  const handleClick = () => {
    clickCount++
    if (clickCount === 1) {
      singleClickTimer = setTimeout(() => {
        clickCount = 0
        // If within proper time range, consider it as fast click
        if (Date.now() - lastClickTime >= timeBetweenClicks) {
          onClick()
        }
        lastClickTime = Date.now()
      }, timeBetweenClicks)
    } else if (clickCount === 2) {
      clearTimeout(singleClickTimer)
      clickCount = 0
      onDoubleClick()
    }
  }
  return { handleClick }
}
