import glob from 'glob'

const options = {
    ignore: 'node_modules*',
}
const eslints = glob.sync('**/.eslintrc*', options)
const tsconfigs = glob.sync('**/tsconfig.json', options)

console.log('eslints: ', eslints)
console.log('tsconfigs: ', tsconfigs)
