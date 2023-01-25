/** @type {import('tailwindcss').Config} */
module.exports = {
    content: ['src/**/*.rs'],
    theme: {
        extend: {
            colors: {
                typeName: '#9640da',
                moduleName: '#a239e2',
                methodsHeader: '#1f71d3',
                methodName: '#1f71d3',
                typesHeader: '#1f71d3',
                examplesHeader: '#6da85e',
                importantBackground: '#edefe7',
                infoBackground: '#e6f1f8',
                exampleBackground: '#e6f1f8',
                docsBackground: '#fcfeff',
                docsText: '#434343',
                tagBackground: '#f5f5f5',
            },
            opacity: {
                85: '.85',
                34: '.34',
            },
        },
    },
    plugins: [],
}
