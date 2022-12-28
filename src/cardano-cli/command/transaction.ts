import { CommandBuilder } from "./command-builder.js";
import { Command } from "./command.js";
import { BuildRaw } from "./transaction/build-raw.js";
import { Build } from "./transaction/build.js";
import { CalculateMinFee } from "./transaction/calculate-min-fee.js";
import { Sign } from "./transaction/sign.js";
import { Submit } from "./transaction/submit.js";
import { TxId } from "./transaction/tx-id.js";

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

  calculateMinFee(builder?: CommandBuilder<CalculateMinFee>): CalculateMinFee {
    let command = new CalculateMinFee(this.getCommand());
    if (builder) {
      command = builder(command);
    }
    return command;
  }

  sign(builder?: CommandBuilder<Sign>): Sign {
    let command = new Sign(this.getCommand());
    if (builder) {
      command = builder(command);
    }
    return command;
  }
  submit(builder?: CommandBuilder<Submit>): Submit {
    let command = new Submit(this.getCommand());
    if (builder) {
      command = builder(command);
    }
    return command;
  }
  txId(builder?: CommandBuilder<TxId>): TxId {
    let command = new TxId(this.getCommand());
    if (builder) {
      command = builder(command);
    }
    return command;
  }
}
