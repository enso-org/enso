/** @file Interfaces common to multiple modules. */

interface StringConfig {
    [key: string]: StringConfig | string
}

interface AppRunner {
    tryStopApp: () => void
    runApp: (config?: StringConfig) => Promise<void>
}
