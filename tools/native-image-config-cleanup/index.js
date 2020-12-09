const fs = require("fs");
const path = require("path");
const process = require("child_process");

/// List of configs to clean.
const configPaths = [
  "../../engine/launcher/src/main/resources/META-INF/native-image/org/enso/launcher",
  "../../lib/scala/project-manager/src/main/resources/META-INF/native-image/org/enso/projectmanager",
];

/// Checks if the entry is ephemeral (contains a pointer in its name).
function isEntryEphemeralClass(entry) {
  const name = entry["name"];
  if (name === undefined) {
    return false;
  }
  return name.indexOf("/0x00") >= 0;
}

/// Sorts the list of entries in a predictable order.
function sortEntries(entries) {
  const copy = Array.from(entries);
  copy.sort((first, second) => {
    const firstName = first["name"];
    const secondName = second["name"];
    if (firstName !== undefined && secondName !== undefined) {
      return firstName < secondName;
    } else if (firstName === undefined && secondName === undefined) {
      return JSON.stringify(first) < JSON.stringify(second);
    } else {
      return firstName === undefined;
    }
  });
  return copy;
}

/// Removes ephemeral classes from the reflection config and ensures it has a
/// stable order of entries.
function cleanReflectionConfig(reflectConfigPath) {
  const data = fs.readFileSync(reflectConfigPath, "utf-8");
  const parsed = JSON.parse(data);
  const withoutEphemeral = parsed.filter(
    (entry) => !isEntryEphemeralClass(entry)
  );
  const sorted = sortEntries(withoutEphemeral);
  const serialized = JSON.stringify(sorted);
  const hasChanges = serialized !== JSON.stringify(parsed);
  if (hasChanges) {
    fs.writeFileSync(reflectConfigPath, serialized);
    console.log("Rewritten " + reflectConfigPath);
  } else {
    console.log("No changes in " + reflectConfigPath);
  }
}

/// Runs prettier on the provided path.
function runPrettier(configPath) {
  console.log("Running prettier for " + configPath);
  process.spawn("npx", ["prettier", "--write", configPath], {
    stdio: "inherit",
  });
}

configPaths.forEach(function (configPath) {
  cleanReflectionConfig(path.join(configPath, "reflect-config.json"));
  runPrettier(configPath);
});
