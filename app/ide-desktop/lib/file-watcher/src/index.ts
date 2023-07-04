import yargs from 'yargs'
import { hideBin } from 'yargs/helpers'
import chokidar from 'chokidar'

const args = yargs(hideBin(process.argv))
    .option('path', {
        alias: 'p',
        type: 'string',
        default: '.',
        description: 'File path to watch.',
    })
    .option('verbose', {
        alias: 'v',
        type: 'boolean',
        default: false,
        description:
            'If false, only reports add, change, and unlink events. If true, reports all events.',
    })
    .parseSync()

if (args.verbose) {
    chokidar.watch(args.path).on('all', (event, path) => {
        console.log({ event, path })
    })
} else {
    chokidar
        .watch(args.path)
        // Add event is emitted whenever a new file is added to the directory being watched.
        // This also triggers when the program first starts if there are files present in the directory.
        .on('add', path => console.log({ event: 'add', path }))
        // Change event is emitted whenever a file is changed.
        .on('change', path => console.log({ event: 'change', path }))
        // Unlink event is emitted whenever a file is removed from the directory being watched.
        .on('unlink', path => console.log({ event: 'unlink', path }))
}
