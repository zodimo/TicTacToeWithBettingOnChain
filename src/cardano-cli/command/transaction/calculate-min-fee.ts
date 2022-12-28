import { config } from "../../../cardano-cli-config.js";
import { Command } from "../command.js";
import { Network } from "../network.js";
import { ProtocolParamsFile } from "../shared/protocol-params-file.js";

/*
Usage: cardano-cli transaction calculate-min-fee --tx-body-file FILE
            [--mainnet | --testnet-magic NATURAL]
            (--genesis FILE | --protocol-params-file FILE)
            --tx-in-count NATURAL
            --tx-out-count NATURAL
            --witness-count NATURAL
            [--byron-witness-count NATURAL]
*/

export class CalculateMinFee extends Command {
  private network: Network;
  private txBodyFile?: string;
  private protocolParamsFile?: ProtocolParamsFile;

  private txInCount: number;
  private txOutCount: number;
  private witnessCount: number;
  private byronWitnessCount?: number;

  constructor(private commandPrefix: string) {
    super();
    this.network = config.getNetwork();
    this.txInCount = 0;
    this.txOutCount = 0;
    this.witnessCount = 0;
  }

  withTxBodyFile(txBodyFile: string): CalculateMinFee {
    this.txBodyFile = txBodyFile;
    return this;
  }

  withNetwork(network: Network): CalculateMinFee {
    this.network = network;
    return this;
  }

  withProtocolParamsFile(
    protocolParamsFile: ProtocolParamsFile
  ): CalculateMinFee {
    this.protocolParamsFile = protocolParamsFile;
    return this;
  }

  withTxInCount(txInCount: number): CalculateMinFee {
    this.txInCount = txInCount;
    return this;
  }
  withTxOutCount(txOutCount: number): CalculateMinFee {
    this.txOutCount = txOutCount;
    return this;
  }
  withWitnessCount(witnessCount: number): CalculateMinFee {
    this.witnessCount = witnessCount;
    return this;
  }
  withByronWitnessCount(byronWitnessCount: number): CalculateMinFee {
    this.byronWitnessCount = byronWitnessCount;
    return this;
  }

  getCommand(): string {
    let ouput: string[] = [this.commandPrefix, "calculate-min-fee"];

    if (this.txBodyFile) {
      ouput.push(`--tx-body-file ${this.txBodyFile}`);
    }
    if (this.network) {
      ouput.push(this.network.asParameter());
    }

    if (this.protocolParamsFile) {
      ouput.push(this.protocolParamsFile.asParameter());
    }

    ouput.push(`--tx-in-count ${this.txInCount}`);
    ouput.push(`--tx-out-count ${this.txOutCount}`);
    ouput.push(`--witness-count ${this.witnessCount}`);
    if (this.byronWitnessCount) {
      ouput.push(`--byron-witness-count ${this.byronWitnessCount}`);
    }

    return ouput.join(" ");
  }
}
