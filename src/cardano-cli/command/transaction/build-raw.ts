import { Era } from "../../era.js";
import { Command } from "../command.js";
import { config } from "../../../cardano-cli-config.js";
import { CommandBuilder } from "../command-builder.js";
import { TxInParameter } from "./build-raw/tx-in.js";

export class BuildRaw extends Command {
  private era: Era;
  private txInList: TxInParameter[];

  constructor(private commandPrefix: string) {
    super();
    this.era = config.getEra();
    this.txInList = [];
  }

  getTxInList(): TxInParameter[] {
    return this.txInList;
  }
  withTxIn(txIn: TxInParameter): BuildRaw {
    this.txInList.push(txIn);
    return this;
  }

  getCommand(): string {
    let ouput: string[] = [this.commandPrefix, "build-raw"];
    if (this.era) {
      ouput.push(this.era.asParameter());
    }
    ouput.push(...this.txInList.map((txInParameter)=>txInParameter.asParameter()))
    return ouput.join(" ");
  }

  withEra(era: Era): BuildRaw {
    this.era = era;
    return this;
  }
}
