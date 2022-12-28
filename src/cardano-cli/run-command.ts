import { execSync } from "child_process";
import { config } from "../cardano-cli-config.js";

export const runCommand: (command: string) => string = (command: string) => {
  if (config.getDebug()) {
    console.log("DEBUG: " + command);
  }
  return execSync(command).toString().trim();
};
