/** @file Configuration for Tailwind. */
/** @type {import('tailwindcss').Config} */
module.exports = {
    content: ['../dashboard/src/authentication/src/**/*.tsx'],
    theme: {
        extend: {},
    },
    corePlugins: {
        preflight: false,
    },
    plugins: [],
}
