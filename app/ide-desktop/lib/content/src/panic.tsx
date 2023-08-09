/** @file This file defines a component that is responsible for displaying a user-facing message
 * about an application crash (panic). The component is used as a body of a Toastify toast. */

import * as React from 'react'
import * as toastify from 'react-toastify'

import * as detect from 'enso-common/src/detect'

/** Props for `InternalPanicMessage` component. */
interface InternalPanicMessageProps {
    /** The panic message with stack trace. Usually big multiline text. */
    message: string
    /** A callback to trigger application restart. */
    restart: () => void
}

/** A component displaying panic message inside a toast. */
function InternalPanicMessage(props: InternalPanicMessageProps) {
    return (
        <div className="flex flex-col">
            <h1 className="text-xl font-semibold">Enso has crashed.</h1>
            <p className="mt-3">
                Enso has encountered a critical error and needs to be restarted. This is a bug, and
                we would appreciate it if you could report it to us.
            </p>
            <p>Please include following panic message in your report:</p>
            <pre className="overflow-auto mt-2 p-2 bg-gray-200 text-gray-800 border rounded border-gray-400 max-h-half-screen">
                {props.message}
            </pre>
            <div className="flex flex-row mt-2 gap-2 justify-end">
                <button
                    className="text-sm border text-gray-800 bg-gray-100 hover:bg-gray-200 rounded p-2 transition"
                    type="submit"
                    onClick={props.restart}
                >
                    Restart
                </button>
                <a
                    className="text-sm border text-white bg-indigo-600 hover:bg-indigo-700 rounded p-2 transition"
                    href={bugReportUrl(props.message)}
                    target="_blank"
                    rel="noreferrer"
                >
                    Report
                </a>
            </div>
        </div>
    )
}

/** Generate an URL to GitHub issue report template, prefilling as much information as possible. */
function bugReportUrl(message: string) {
    const version = BUILD_INFO.version.endsWith('-dev') ? BUILD_INFO.commit : BUILD_INFO.version
    const browserVersion = detect.isRunningInElectron() ? 'standalone' : navigator.userAgent
    const logs = bugReportLogs(message)

    const reportUrl = new URL('https://GitHub.com/enso-org/enso/issues/new')
    const params = reportUrl.searchParams
    params.append('labels', '--bug,triage')
    params.append('template', 'bug-report.yml')
    params.append('title', 'Panic report')
    params.append('enso-version', version)
    params.append('browser-version', browserVersion)
    params.append('logs', logs)
    return reportUrl.toString()
}

/** Generate a pre-filled log message for GitHub issue report. The output of this function is later
 * encoded and used as an URL query parameter.
 */
function bugReportLogs(message: string) {
    // Due to URL length limitation, we need to truncate the panic massage that is passed to the
    // GitHub issue template. The stacktrace is cut off line per line, up to the limit. The limit
    // is chosen somewhat conservatively, but within the limits that GitHub allows. Verified
    // empirically.
    const maxMessageLength = 5000

    const lastLineEnd = message.lastIndexOf('\n', maxMessageLength)
    const truncatedMessage = message.slice(0, lastLineEnd < 0 ? maxMessageLength : lastLineEnd)
    return '```\n' + truncatedMessage + '\n```\n'
}

/** Display a toast with panic message. */
export function displayPanicMessageToast(message: string, restartApp: () => void) {
    const restart = () => {
        restartApp()
        toastify.toast.dismiss(toastId)
    }
    const element = <InternalPanicMessage message={message} restart={restart} />
    const toastId = toastify.toast.error(element, {
        closeButton: false,
        autoClose: false,
        style: {
            // Allow the toast to fill the screen almost completely, leaving a small margin.
            margin: '0 calc((100% - min(100vw - 2rem, 1200px)) / 2)',
            maxHeight: 'calc(100vh - 4rem)',
        },
        bodyStyle: {
            alignItems: 'flex-start',
            overflow: 'hidden',
        },
    })
}
