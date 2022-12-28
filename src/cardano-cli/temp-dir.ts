import fs from "fs";
import { config } from "../cardano-cli-config.js";
import { runCommand } from "./run-command.js";

export const createTempFilename: (filename: string) => string = (filename) => {
  return `${getTempDir()}/${filename}`;
};

export const getTempDir = () => {
  const workingDirectory = config.getWorkingDirectory();
  const paths: string[] = [workingDirectory];

  if (!workingDirectory.endsWith("/")) {
    paths.push("/");
  }
  paths.push("tmp");

  return paths.join("");
};

export const ensureTempDirectoryExists = () => {
  const tempDirPath = getTempDir();
  if (!fs.existsSync(tempDirPath)) runCommand(`mkdir -p ${tempDirPath}`);
};
