const fs = require("fs");

const path = "build.sbt";
const version = process.argv[2];

const content = fs.readFileSync(path, { encoding: "utf-8" });
const updated = content.replace(
  /val ensoVersion.*= ".*"/g,
  'val ensoVersion = "' + version + '"'
);
fs.writeFileSync(path, updated);

console.log("Updated build version to " + version);
