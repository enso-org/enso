/** @file Configuration for Tailwind. */
/** @type {import('tailwindcss').Config} */
module.exports = {
    content: ['./src/**/*.tsx'],
    important: '#dashboard',
    theme: {
        extend: {},
    },
    corePlugins: {
        preflight: false,
    },
    plugins: [],
}
