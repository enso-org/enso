const IGNORED_DEPS = [
  'react-native-url-polyfill',
  'react-native-get-random-values',
  'pug',
  'y-leveldb',
]

const unusedIgnores = new Set(IGNORED_DEPS)
module.exports.hooks = {
  readPackage: (pkg, context) => {
    for (const ignored of IGNORED_DEPS) {
      if (pkg.dependencies[ignored] || pkg.optionalDependencies[ignored]) {
        delete pkg.dependencies[ignored]
        delete pkg.optionalDependencies[ignored]
        context.log(`Ignoring dependency ${ignored} in ${pkg.name}`)
        unusedIgnores.delete(ignored)
      }
    }
    return pkg
  },
  afterAllResolved(lockfile, context) {
    if (unusedIgnores.size > 0) {
      context.log(`Unused dependency ignore declarations: ${Array.from(unusedIgnores).join(', ')}`)
    }
    return lockfile
  },
}
