import { Mixpanel } from 'mixpanel-browser'
import { LogLevel, Consumer } from 'ensogl_app'

// ==============
// === Logger ===
// ==============

export class MixpanelLogger extends Consumer {
    mixpanel: Mixpanel
    groups: (string | null)[] = []

    constructor(debug: boolean) {
        super()
        this.mixpanel = require('mixpanel-browser')
        this.mixpanel.init(
            '5b541aeab5e08f313cdc1d1bbebc12ac',
            { debug, api_host: 'https://api-eu.mixpanel.com' },
            ''
        )
    }

    message(level: LogLevel, ...args: any[]) {
        if (args.length > 0) {
            this.mixpanelLog(level, args[0].toString(), args.slice(1))
        }
    }

    group(...args: any[]) {
        if (args.length > 0) {
            let message = args[0].toString()
            this.mixpanelLog('log', `start: ${message}`, args.slice(1))
            this.groups.push(message)
        } else {
            this.groups.push(null)
        }
    }

    groupCollapsed(...args: any[]) {
        this.group(...args)
    }

    groupEnd(...args: any[]) {
        let message = this.groups.pop()
        if (message != null) {
            this.mixpanelLog('log', `end: ${message}`, args)
        }
    }

    mixpanelLog(level: LogLevel, message: string, data: any) {
        const trimmedMessage = this.trim_message(message)
        try {
            let payload: any = { level }
            if (data != null) {
                // FIXME: make data passing more intelligent here. If arg is object, just pass it to mixpanel with stringified vlaues - to be checked in mixpanel manual
                payload.data = this.trim_message(JSON.stringify(data))
            }
            this.mixpanel.track(trimmedMessage, payload, response => {
                // Mixpanel returns 1 on success. To learn more, see
                // https://mixpanel.com/help/reference/http#tracking-events
                if (typeof response == 'number') {
                    if (response == 0) {
                        console.error('Failed to log the event with Mixpanel.')
                    }
                } else {
                    if (response.status == 0) {
                        console.warn(`Failed to log the event with Mixpanel: '${response.error}'.`)
                    }
                }
            })
        } catch {
            console.warn(`Failed to log the event with Mixpanel: '${trimmedMessage}'.`)
        }
    }

    trim_message(message: string): string {
        const MAX_MESSAGE_LENGTH = 500
        let trimmed = message.substring(0, MAX_MESSAGE_LENGTH)
        if (trimmed.length < message.length) {
            trimmed += '...'
        }
        return trimmed
    }

    identify(uniqueId: string) {
        this.mixpanel.identify(uniqueId)
    }
}
