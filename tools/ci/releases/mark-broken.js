#!/usr/bin/env node
const fs = require("fs");
let usage = `Usage: mark-broken.js PATH TAG

Updates the release list at PATH by adding the broken mark to the release with
tag TAG.`;

if (process.argv.length != 4) {
  console.log(usage);
  process.exit(2);
}

let path = process.argv[2];
let tag = process.argv[3];

fs.readFile(path, "utf8", (err, data) => {
  if (err) {
    console.error(err);
    process.exit(2);
  }

  let root = JSON.parse(data);
  let release = root["releases"].find((release) => release["tag"] == tag);
  if (release === undefined) {
    console.error(`Release '${tag}' is not present in the metadata.`);
    console.error("No changes written.");
    process.exit(1);
  }

  if (release["assets"].includes("broken")) {
    console.error("Broken mark is already present in the metadata.");
  } else {
    release["assets"].push("broken");
    fs.writeFile(path, JSON.stringify(root, null, 1) + "\n", (err) => {
      if (err) {
        console.error(err);
        process.exit(2);
      } else {
        console.error("Broken mark has been added.");
      }
    });
  }
});
