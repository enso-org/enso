/** @file typescript declaration on dev environment  */
declare module '*.yaml' {
    const data: any
    export default data
}

declare const BUNDLED_ENGINE_VERSION: string
declare const PROJECT_MANAGER_IN_BUNDLE_PATH: string
