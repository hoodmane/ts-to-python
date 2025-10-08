import { Logger } from "tslog";

// Suppress logs below warning unless we're in a node test or TS_TO_PYTHON_DEBUG
// is set.
let minLevel: number | undefined = 4;
if (process.env["TS_TO_PYTHON_DEBUG"] || process.env["NODE_TEST_CONTEXT"]) {
  minLevel = undefined;
}

export const logger = new Logger({
  prettyLogTemplate: "{{logLevelName}}\t{{fileNameWithLine}}\t",
  minLevel,
});
