import { Era } from "../../era.js";
import { Command } from "../command.js";
import { config } from "../../../cardano-cli-config.js";

export class Build extends Command {
  private era: Era;
  constructor(private commandPrefix: string) {
    super();
    this.era = config.getEra();
  }

  getCommand(): string {
    let ouput: string[] = [this.commandPrefix, "build"];
    if (this.era) {
      ouput.push(this.era.asParameter());
    }
    return ouput.join(" ");
  }

  withEra(era: Era): Build {
    this.era = era;
    return this;
  }
}
