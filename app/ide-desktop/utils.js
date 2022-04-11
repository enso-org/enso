function require_env(name) {
    return (
        process.env[name] ??
        (() => {
            throw Error(`Missing ${name} environment variable.`)
        })()
    )
}

function require_env_path_exist(name) {
    const value = require_env(name)
    if (path.existsSync(value)) return value
    else throw Error(`File with path ${value} read from environment variable ${name} is missing.`)
}

module.exports = { require_env, require_env_path_exist }
