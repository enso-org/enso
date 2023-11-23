export function useDoubleClick(onClick: Function, onDoubleClick: Function) {
  const timeBetweenClicks = 200
  let clickCount = 0
  let singleClickTimer: ReturnType<typeof setTimeout>

  const handleClick = () => {
    clickCount++
    if (clickCount === 1) {
      onClick()
      singleClickTimer = setTimeout(() => {
        clickCount = 0
      }, timeBetweenClicks)
    } else if (clickCount === 2) {
      clearTimeout(singleClickTimer)
      clickCount = 0
      onDoubleClick()
    }
  }
  return { handleClick }
}
