/** @file Prettier configuration. */
// @ts-check
/** @type {import("@ianvs/prettier-plugin-sort-imports").PrettierConfig} */
module.exports = {
  overrides: [
    {
      files: ['*.[j|t]s', '*.[j|t]sx', '*.m[j|t]s', '*.c[j|t]s'],
      options: {
        // eslint-disable-next-line @typescript-eslint/no-magic-numbers
        printWidth: 100,
        tabWidth: 2,
        semi: false,
        singleQuote: true,
        trailingComma: 'es5',
        arrowParens: 'avoid',
        plugins: ['@ianvs/prettier-plugin-sort-imports'],
        // This plugin's options
        importOrder: [
          '^react$',
          '',
          '<THIRD_PARTY_MODULES>',
          '',
          '^enso-',
          '',
          '^#[/]App',
          '^#[/]appUtils',
          '',
          '^#[/]hooks[/]',
          '',
          '^#[/]providers[/]',
          '',
          '^#[/]events[/]',
          '',
          '^#[/]pages[/]',
          '',
          '^#[/]layouts[/]',
          '',
          '^#[/]components[/]',
          '',
          '^#[/]services[/]',
          '',
          '^#[/]utilities[/]',
          '',
          '^#[/]authentication[/]',
          '',
          '^[.]',
        ],
        importOrderParserPlugins: ['typescript', 'jsx', 'importAssertions'],
        importOrderTypeScriptVersion: '5.0.0',
      },
    },
  ],
}
