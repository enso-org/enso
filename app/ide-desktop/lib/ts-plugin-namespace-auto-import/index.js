/** @file A plugin to change auto-imports to use `import * as module` instead of `import {}`. */

/** A TypeScript compiler plugin. */
module.exports = function init() {
    /** Turn a module name into a valid identifier.
     * @param {string} name - module name */
    const normalizeModuleName = name =>
        name.replace(/^\W+|\W+(.?)/g, (_, a) => String(a ?? '').toUpperCase())

    /** Create the plugin.
     * @param {import('typescript/lib/tsserverlibrary').server.PluginCreateInfo} info - Plugin utilities. */
    const create = info => {
        /** @type {import('typescript/lib/tsserverlibrary').LanguageService} */
        // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
        const proxy = Object.create(null)
        const oldLS = info.languageService
        for (const [k, v] of Object.entries(oldLS)) {
            // @ts-expect-error Runtime reflection is not type-safe.
            // eslint-disable-next-line @typescript-eslint/no-unsafe-return, @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
            proxy[k] = (...args) => v.apply(oldLS, args)
        }

        proxy.getCompletionsAtPosition = (fileName, position, options) => {
            const result = info.languageService.getCompletionsAtPosition(
                fileName,
                position,
                options
            )
            for (const completion of result?.entries ?? []) {
                if (completion.hasAction && /\bexport\b/.test(completion.kindModifiers ?? '')) {
                    const moduleName = completion.data?.moduleSpecifier?.match(/[^/]+$/)?.[0] ?? ''
                    if (moduleName) {
                        completion.name = `${normalizeModuleName(moduleName)}.${completion.name}`
                    }
                }
            }
            return result
        }

        proxy.getCompletionEntryDetails = (
            fileName,
            position,
            entryName,
            formatOptions,
            source,
            preferences,
            data
        ) => {
            const result = info.languageService.getCompletionEntryDetails(
                fileName,
                position,
                entryName.replace(/^.+[.]/, ''),
                formatOptions,
                source,
                preferences,
                data
            )
            for (const action of result?.codeActions ?? []) {
                if (action.description.startsWith('Add import from ')) {
                    for (const change of action.changes) {
                        for (const textChange of change.textChanges) {
                            textChange.newText = textChange.newText.replace(
                                /^(import ){.*}( from '(?:.*[/])?(.*)')/,
                                (_, prefix, suffix, moduleName) =>
                                    `${prefix}* as ${normalizeModuleName(
                                        String(moduleName)
                                    )}${suffix}`
                            )
                        }
                    }
                }
            }
            return result
        }

        return proxy
    }

    return { create }
}
