/** @file Helpers for rendering execution usage values. */

/** Format uptime summary value in short human-readable form. */
export function formatUptime(totalSeconds: number) {
  const secondsInHour = 3600
  const secondsInMinute = 60
  const hours = Math.floor(totalSeconds / secondsInHour)
  const minutes = Math.floor((totalSeconds % secondsInHour) / secondsInMinute)
  const seconds = totalSeconds % secondsInMinute

  if (hours === 0 && minutes === 0) {
    return `${seconds}s`
  }

  if (hours === 0) {
    return `${minutes}m ${seconds}s`
  }

  const formattedMinutes = String(minutes).padStart(2, '0')

  if (seconds === 0) {
    return `${hours}h ${formattedMinutes}m`
  }

  return `${hours}h ${formattedMinutes}m ${String(seconds).padStart(2, '0')}s`
}
