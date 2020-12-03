const fs = require("fs");
const path = require("path");
const process = require('child_process');

const configPaths = ["../../engine/launcher/native-image-config", "../../lib/scala/project-manager/native-image-config"];

function isEntryEphemeralClass(entry) {
  const name = entry["name"];
	if (name === undefined) {
		return false;
	}
	return name.indexOf("/0x00") >= 0;
}

function cleanEphemeralClasses(reflectConfigPath) {
	const data = fs.readFileSync(reflectConfigPath, "utf-8");
	const parsed = JSON.parse(data);
	const withoutEphemeral = parsed.filter(entry => !isEntryEphemeralClass(entry));
	if (withoutEphemeral.length == parsed.length) {
		console.log("No ephemeral classes found for " + reflectConfigPath + ", no changes.");
	} else {
	  const serialized = JSON.stringify(withoutEphemeral);
		fs.writeFileSync(reflectConfigPath, serialized);
		console.log("Rewritten " + reflectConfigPath);
	}
}

function runPrettier(configPath) {
	console.log("Running prettier for " + configPath);
	process.spawn("npx", ["prettier", "--write", configPath], {stdio: "inherit"});
}

configPaths.forEach(function(configPath) {
	cleanEphemeralClasses(path.join(configPath, "reflect-config.json"));
	runPrettier(configPath);
});
