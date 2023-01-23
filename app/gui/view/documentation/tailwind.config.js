/** @type {import('tailwindcss').Config} */
module.exports = {
    content: ['src/**/*.rs'],
    theme: {
        extend: {
            colors: {
                type: '#a239e2',
                module: '#a239e2',
                arguments: '#e0bdf7',
                methods: '#0273da',
                types: '#0273da',
                examples: '#59aa54',
                important: '#edefe7',
                info: '#e6f1f8',
            },
        },
    },
    plugins: [],
}
