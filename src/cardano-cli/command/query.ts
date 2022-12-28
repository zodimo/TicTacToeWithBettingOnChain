import { Command } from "./command.js";
import { CommandBuilder } from "./command-builder.js";
import { ProtocolParameters } from "./query/protocol-parameters.js";
import { Tip } from "./query/tip.js";
import { Utxo } from "./query/utxo.js";

export class Query extends Command {
  constructor(private commandPrefix: string) {
    super();
  }
  getCommand(): string {
    return `${this.commandPrefix} query`;
  }

  tip(builder?: CommandBuilder<Tip>): Tip {
    let command = new Tip(this.getCommand());
    if (builder) {
      command = builder(command);
    }
    return command;
  }

  protocolParameters(
    builder?: CommandBuilder<ProtocolParameters>
  ): ProtocolParameters {
    let command = new ProtocolParameters(this.getCommand());
    if (builder) {
      command = builder(command);
    }
    return command;
  }

  utxo(builder?: CommandBuilder<Utxo>): Utxo {
    let command = new Utxo(this.getCommand());
    if (builder) {
      command = builder(command);
    }
    return command;
  }
}
