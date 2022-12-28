import { Era } from "../../era.js";
import { Command } from "../command.js";
import { config } from "../../../cardano-cli-config.js";
import { CommandBuilder } from "../command-builder.js";
import { TxInParameter } from "./build-raw/tx-in.js";
import { TxOutParameter } from "./build-raw/tx-out.js";
import { OutFile } from "../shared/out-file.js";
import { Fee } from "../shared/fee.js";
import { ProtocolParamsFile } from "../shared/protocol-params-file.js";

export class BuildRaw extends Command {
  private era: Era;
  private txInList: TxInParameter[];
  private txOutList: TxOutParameter[];
  private outFile?: OutFile;
  private fee: Fee;
  private protocolParamsFile?: ProtocolParamsFile;

  constructor(private commandPrefix: string) {
    super();
    this.era = config.getEra();
    this.txInList = [];
    this.txOutList = [];
    this.fee = new Fee(0);
  }

  withOutFile(outFile: OutFile): BuildRaw {
    this.outFile = outFile;
    return this;
  }

  withTxIn(txIn: TxInParameter): BuildRaw {
    this.txInList.push(txIn);
    return this;
  }

  withTxOut(txOut: TxOutParameter): BuildRaw {
    this.txOutList.push(txOut);
    return this;
  }

  withFee(fee: Fee): BuildRaw {
    this.fee = fee;
    return this;
  }

  withProtocolParamsFile(protocolParamsFile: ProtocolParamsFile): BuildRaw {
    this.protocolParamsFile = protocolParamsFile;
    return this;
  }

  getCommand(): string {
    let ouput: string[] = [this.commandPrefix, "build-raw"];
    //era
    if (this.era) {
      ouput.push(this.era.asParameter());
    }
    //tx-in
    ouput.push(
      ...this.txInList.map((txInParameter) => txInParameter.asParameter())
    );
    //tx-out
    ouput.push(
      ...this.txOutList.map((txOutParameter) => txOutParameter.asParameter())
    );

    //fee
    if (this.fee) {
      ouput.push(this.fee.asParameter());
    }

    if (this.protocolParamsFile) {
      ouput.push(this.protocolParamsFile.asParameter());
    }
    //out-file
    if (this.outFile) {
      ouput.push(this.outFile.asParameter());
    }
    return ouput.join(" ");
  }

  withEra(era: Era): BuildRaw {
    this.era = era;
    return this;
  }
}
