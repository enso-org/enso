/** @file Package info. */

import { logger } from 'runner/log'

// ===================
// === PackageInfo ===
// ===================

/** Package info. It can also contain info provided by the user of this library. */
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
            logger.with('Package info.', () => {
                for (const [key, value] of Object.entries(this)) {
                    if (value != null) {
                        logger.log(`${key}: ${value}`)
                    }
                }
            })
        }
    }
}
