import { CommandBuilder } from "./command-builder.js";
import { Command } from "./command.js";
import { BuildRaw } from "./transaction/build-raw.js";
import { Build } from "./transaction/build.js";

export class Transaction extends Command {
  constructor(private commandPrefix: string) {
    super();
  }

  getCommand(): string {
    return `${this.commandPrefix} transaction`;
  }


  buildRaw(builder?: CommandBuilder<BuildRaw>): BuildRaw {
    let command = new BuildRaw(this.getCommand());
    if (builder) {
      command = builder(command);
    }
    return command;
  }

  build(builder?: CommandBuilder<Build>): Build {
    let command = new Build(this.getCommand());
    if (builder) {
      command = builder(command);
    }
    return command;
  }
}
