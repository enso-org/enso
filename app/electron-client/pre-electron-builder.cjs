// Ensure a local node_modules exists to prevent electron-builder from trying to install deps.
try {
  const fs = require('fs')
  const path = require('path')
  const execroot = process.env.JS_BINARY__EXECROOT
  // electron-builder uses appDir (cwd), but some flows derive from execroot + package.
  // Create node_modules in both cwd and execroot/app/electron-client best-effort.
  fs.mkdirSync(path.join(process.cwd(), 'node_modules'), { recursive: true })
  if (execroot) {
    fs.mkdirSync(path.join(execroot, 'app', 'electron-client', 'node_modules'), { recursive: true })
  }

  // Copy esbuild outputs from BINDIR into execroot/app/electron-client
  try {
    const bindir = process.env.BAZEL_BINDIR || process.env.JS_BINARY__BINDIR
    if (execroot && bindir) {
      const srcBundle = path.join(execroot, bindir, 'app', 'electron-client', 'bundle')
      const dstBundle = path.join(execroot, 'app', 'electron-client', 'bundle')
      if (fs.existsSync(srcBundle)) {
        fs.cpSync(srcBundle, dstBundle, { recursive: true })
      }
    }
  } catch (_) {}

  // Rewrite --config to absolute execroot path to avoid fs patch readlink on bazel-out.
  if (execroot) {
    const absConfig = path.join(execroot, 'app', 'electron-client', 'electron-builder-config.json')
    const argv = process.argv
    for (let i = 0; i < argv.length; i++) {
      const arg = argv[i]
      if (arg.startsWith('--config=')) {
        argv[i] = `--config=${absConfig}`
        break
      } else if (arg === '--config' && i + 1 < argv.length) {
        argv[i + 1] = absConfig
        break
      }
    }
  }
} catch (_) {}
