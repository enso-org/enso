/** @file Package info. */

// ===================
// === PackageInfo ===
// ===================

/** Package info. It contains info provided by the user. */
export class PackageInfo {
  /** Constructor. */
  constructor(userProvidedInfo?: Record<string, string>) {
    const infoObject = userProvidedInfo ?? {}
    Object.assign(this, infoObject)
  }

  /** Display the current info in the console. */
  display() {
    const entries = Object.entries(this)
    if (entries.length > 0) {
      console.group('Package info.', () => {
        for (const [key, value] of Object.entries(this)) {
          if (value != null) {
            console.log(`${key}: ${value}`)
          }
        }
      })
    }
  }
}
