const fs = require("fs");

const inputPath = process.argv[2];
const outputPath = process.argv[3];

console.log("Extracting release notes from " + inputPath + " to " + outputPath);

/** Returns the part of the text until the second top-level heading (exclusive)
 * in Markdown formatting.
 */
function cutFirstSection(content) {
  const nightlySectionRegex = /^# Enso Next$/gm;
  function findNightlySectionStart(text) {
    return text.search(nightlySectionRegex);
  }
  const regularSectionRegex = /^# Enso .*? \(\d\d\d\d-\d\d-\d\d\)$/gm;
  function findFirstRegularSectionStart(text) {
    return text.search(regularSectionRegex);
  }
  function findNewline(text) {
    return text.indexOf("\n");
  }

  const firstHeading = findNightlySectionStart(content);
  if (firstHeading < 0) {
    throw "Could not find the nightly section, matching " + nightlySectionRegex;
  }

  const restOffset = firstHeading + 2;
  const newLineOffset = findNewline(content.substring(restOffset));
  if (newLineOffset < 0) {
    throw "No content after the section heading";
  }
  const restStart = restOffset + newLineOffset + 1;

  const rest = content.substring(restStart);
  const secondHeading = findFirstRegularSectionStart(rest);
  if (secondHeading < 0) {
    throw (
      "Could not find the first released section, matching" +
      regularSectionRegex
    );
  }

  const firstSectionContent = rest.substring(0, secondHeading);
  return firstSectionContent;
}

try {
  const content = fs.readFileSync(inputPath, { encoding: "utf-8" });
  const nightlyPart = cutFirstSection(content);
  fs.writeFileSync(outputPath, nightlyPart);

  console.log("Created " + outputPath + " with the following content:");
  console.log(nightlyPart);
} catch (exc) {
  console.error(exc);
  process.exit(1);
}
