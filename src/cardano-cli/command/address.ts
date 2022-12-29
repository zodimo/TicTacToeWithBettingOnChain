import { Command } from "./command.js";
import { CommandBuilder } from "./command-builder.js";
import { KeyGen } from "./address/key-gen.js";
import { KeyHash } from "./address/key-hash.js";
import { Build } from "./address/build.js";
import { BuildScript } from "./address/build-script.js";
import { Info } from "./address/info.js";

/*
Usage: cardano-cli address (key-gen | key-hash | build | build-script | info)
*/
export class Address extends Command {
  constructor(private commandPrefix: string) {
    super();
  }
  getCommand(): string {
    return `${this.commandPrefix} query`;
  }

  keyGen(builder?: CommandBuilder<KeyGen>): KeyGen {
    let command = new KeyGen(this.getCommand());
    if (builder) {
      command = builder(command);
    }
    return command;
  }

  keyHash(
    builder?: CommandBuilder<KeyHash>
  ): KeyHash {
    let command = new KeyHash(this.getCommand());
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

  buildScript(builder?: CommandBuilder<BuildScript>): BuildScript {
    let command = new BuildScript(this.getCommand());
    if (builder) {
      command = builder(command);
    }
    return command;
  }

  info(builder?: CommandBuilder<Info>): Info {
    let command = new Info(this.getCommand());
    if (builder) {
      command = builder(command);
    }
    return command;
  }
}
