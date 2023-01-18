/** @file Package info. */

import { logger } from 'runner/log'

// ===================
// === PackageInfo ===
// ===================

/** Package info. It contains the build git hash and git status, both injected during build time.
 * See `bundle.ts` to learn more. It can also contain additional info provided by the user of this
 * library. */
export class PackageInfo {
    // gitHash: string
    // gitStatus: string

    /** Constructor. */
    constructor(userProvidedInfo?: Record<string, string>) {
        const infoObject = userProvidedInfo ?? {}
        // /* eslint @typescript-eslint/no-unsafe-assignment: "off" */
        // // @ts-expect-error
        // this.gitHash = GIT_HASH
        // /* eslint @typescript-eslint/no-unsafe-assignment: "off" */
        // // @ts-expect-error
        // this.gitStatus = GIT_STATUS
        Object.assign(this, infoObject)
    }

    /** Display the current info in the console. */
    display() {
        logger.with('Package info.', () => {
            for (const [key, value] of Object.entries(this)) {
                if (value != null) {
                    logger.log(`${key}: ${value}`)
                }
            }
        })
    }
}
