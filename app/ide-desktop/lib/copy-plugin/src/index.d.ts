/** @file Type definitions for the `copy-plugin` module. */
import * as esbuild from 'esbuild'

export function create(filesProvider: () => AsyncGenerator<string>): esbuild.Plugin
