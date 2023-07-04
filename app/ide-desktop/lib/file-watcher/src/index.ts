import yargs from "yargs"
import { hideBin } from "yargs/helpers"
import chokidar from 'chokidar';

const args = yargs(hideBin(process.argv))
    .option('path', {
        alias: 'p',
        type: 'string',
        default: '.',
        description: 'File path to watch'
    })
    .option('verbose', {
        alias: 'v',
        type: 'boolean',
        default: false,
        description: 'If false, only reports add, change, and unlink events.If true, reports all events.'
    })
    .parseSync()


// console.log(`Starting to watch "${args.path}" for changes...`);

if (args.verbose) {
    ``
    chokidar.watch(args.path).on('all', (event, path) => {
        console.log({ event, path });
    });
} else {
    chokidar.watch(args.path)
        .on('add', path => console.log({ event: 'add', path }))
        .on('change', path => console.log({ event: 'change', path }))
        .on('unlink', path => console.log({ event: 'unlink', path }));
}
