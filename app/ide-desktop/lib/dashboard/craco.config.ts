/** @file Configuration file for the `craco` package.
 * 
 * `craco` is used in place of the default `react-scripts` package provided by `create-react-app`.
 * `craco` is a wrapper around `react-scripts` that allows us to customize the build process. We use
 * it to disable the `ModuleScopePlugin`. The `ModuleScopePlugin` prevents importing modules outside
 * of the `src` directory. This package imports files from sibling packages in the monorepo, so the
 * `ModuleScopePlugin` must be disabled.
 * 
 * See: https://stackoverflow.com/a/60353355. */

import * as craco from '@craco/types';
import * as webpack from 'webpack'

// =================
// === Constants ===
// =================

/** Name of the `ModuleScopePlugin` plugin. */
const MODULE_SCOPE_PLUGIN = 'ModuleScopePlugin';

// ==============================
// === Disable Plugin By Name ===
// ==============================

/** Finds and disables the plugin with the given `name` in the Webpack configuration, if the plugin
 * exists. */
const disablePluginByName = (webpackConfig: webpack.Configuration, name: string): void => {
    const plugins = webpackConfig.resolve?.plugins;
    if (!plugins) {
        return;
    }

    const pluginIndex = plugins.findIndex(
        ({ constructor }) => constructor && constructor.name === name
    );
    if (pluginIndex !== -1) {
        plugins.splice(pluginIndex, 1);
    }
}

// ====================
// === Craco Config ===
// ====================

module.exports = {
    webpack: {
        configure: webpackConfig => {
            disablePluginByName(webpackConfig, MODULE_SCOPE_PLUGIN)
            return webpackConfig;
        }
    }
} as craco.CracoConfig;
