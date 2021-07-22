#!/usr/bin/env node
const express = require("express");
const path = require('path');
const os = require('os');
const multer = require("multer");
const compression = require("compression");
const yargs = require("yargs");

const argv = yargs
  .usage(
    "$0",
    "Allows to host Enso libraries and editions from the local filesystem through HTTP."
  )
  .option("port", {
    description: "The port to listen on.",
    type: "number",
    default: 8080,
  })
  .option("root", {
    description:
      "The root of the repository. It should contain a `libraries` or `editions` directory. See the documentation for more details.",
    type: "string",
    default: ".",
  })
  .help()
  .alias("help", "h").argv;

const app = express();
const tmpDir = path.join(os.tmpdir(), "enso-library-repo-uploads");
const upload = multer({ dest: tmpDir });
app.use(compression({ filter: shouldCompress }));
app.post("/upload", upload.any(), handleUpload);
app.use(express.static(argv.root));

console.log(
  `Serving the repository located under ${argv.root} on port ${argv.port}.`
);

app.listen(argv.port);

function shouldCompress(req, res) {
  if (req.path.endsWith(".yaml")) {
    return true;
  }

  return compression.filter(req, res);
}

function handleUpload(req, res) {
  console.log(req.query);
  console.log(req.files);
  console.log(req.body);
  res.json({ message: "Successfully uploaded files" });
}
