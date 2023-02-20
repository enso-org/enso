import { defineConfig } from 'tsup'

export default defineConfig({
    outExtension({ format }) {
        return {
            js: `.js`,
        }
    },
    // FIXME Does not work: https://github.com/egoist/tsup/issues/819
    bundle: true,
    skipNodeModulesBundle: false,
})
