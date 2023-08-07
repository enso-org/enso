interface PanicMessageProps {
    message: string
    restart: () => void
}

export function PanicMessage(props: PanicMessageProps) {
    return (
        <div className="flex flex-col">
            <h1 className="text-xl font-semibold">Enso has crashed.</h1>
            <p className="mt-3">
                Enso has encountered a critical error and needs to be restarted. This is a bug, and
                we would appreciate it if you could report it to us.
            </p>
            <p>Please include following panic message in your report:</p>
            <pre
                className="overflow-auto mt-2 p-2 bg-gray-200 text-gray-800 border rounded border-gray-400"
                style={{ maxHeight: '50vh' }}
            >
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
                    target="_blank"
                    href="https://github.com/enso-org/enso/issues/new?labels=--bug%2Ctriage&template=bug-report.yml"
                    className="text-sm border text-white bg-indigo-600 hover:bg-indigo-700 rounded p-2 transition"
                >
                    Report
                </a>
            </div>
        </div>
    )
}
