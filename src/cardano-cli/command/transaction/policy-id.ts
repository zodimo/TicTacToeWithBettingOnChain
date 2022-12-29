import { Command } from "../command.js";

/*
Usage: cardano-cli transaction policyid --script-file FILE
 */
export class PolicyId extends Command {
  private scriptFile?: string;
  constructor(private commandPrefix: string) {
    super();
  }

  withScriptFile(scriptFile: string): PolicyId {
    this.scriptFile = scriptFile;
    return this;
  }

  getCommand(): string {
    let output: string[] = [this.commandPrefix, "policyid"];

    if (this.scriptFile) {
      output.push(`--script-file ${this.scriptFile}`);
    }

    return output.join(" ");
  }
}
