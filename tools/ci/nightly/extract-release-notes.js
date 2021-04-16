const fs = require('fs');

const inputPath = process.argv[2];
const outputPath = process.argv[3];

console.log("Extracting release notes from " + inputPath + " to " + outputPath);

/** Returns the part of the text until the second top-level heading (exclusive)
  * in Markdown formatting.
  */
function cutFirstSection(content) {
  // TODO [RW] can we assume that '# ' is always the start of a section?
  // This requires that there are no code snippets with comments starting at the
  // beginning of the line.
  function findSectionStart(text) {
    return text.search(/^# /gm);
  }

  const firstHeading = findSectionStart(content);
  if (firstHeading < 0) {
    throw "No sections in file!";
  }
  const restStart = firstHeading + 2;
  const rest = content.substring(restStart);
  const secondHeading = findSectionStart(rest);
  if (secondHeading < 0) {
    // There is only one section.
    return content;
  }
  const secondHeadingOffsetInContent = restStart + secondHeading;
  const firstSectionContent = content.substring(0, secondHeadingOffsetInContent);
  return firstSectionContent;
}

const content = fs.readFileSync(inputPath, {encoding: "utf-8"});
const nightlyPart = cutFirstSection(content);
fs.writeFileSync(outputPath, nightlyPart);

console.log("Created " + outputPath + " with the following content:");
console.log(nightlyPart);
