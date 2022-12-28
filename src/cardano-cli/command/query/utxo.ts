import { config } from "../../../cardano-cli-config.js";
import { Network } from "../network.js";
import { Command } from "../command.js";
import { NodeMode } from "../node-mode.js";
import { Filter } from "./utxo/filter.js";
import { WholeUtxo } from "./utxo/whole-utxo.js";
import { OutFile } from "../shared/out-file.js";

/*
Usage: cardano-cli query utxo 
            [ --shelley-mode
            | --byron-mode [--epoch-slots NATURAL]
            | --cardano-mode [--epoch-slots NATURAL]
            ]
            (--whole-utxo | (--address ADDRESS) | (--tx-in TX-IN))
            (--mainnet | --testnet-magic NATURAL)
            [--out-file FILE]
*/
export class Utxo extends Command {
  private network?: Network;
  private nodeMode?: NodeMode;
  private outFile?: OutFile;
  private filter?: Filter;
  private wholeUtxo?: WholeUtxo;

  constructor(private commandPrefix: string) {
    super();
    this.network = config.getNetwork();
    this.nodeMode = config.getNodeMode();
  }

  withNetwork(network: Network): Utxo {
    this.network = network;
    return this;
  }

  withNodeMode(nodeMode: NodeMode): Utxo {
    this.nodeMode = nodeMode;
    return this;
  }

  withOutFile(outFile: OutFile): Utxo {
    this.outFile = outFile;
    return this;
  }

  withFilter(filter: Filter): Utxo {
    this.filter = filter;
    return this;
  }
  withWholeUtxo(wholeUtxo: WholeUtxo): Utxo {
    this.wholeUtxo = wholeUtxo;
    return this;
  }

  getCommand(): string {
    let ouput: string[] = [this.commandPrefix, "utxo"];
    if (this.nodeMode) {
      ouput.push(this.nodeMode.asParameter());
    }

    if (this.wholeUtxo) {
      ouput.push(this.wholeUtxo.asParameter());
    }

    if (this.filter) {
      ouput.push(this.filter.asParameter());
    }

    if (this.network) {
      ouput.push(this.network.asParameter());
    }
    if (this.outFile) {
      ouput.push(this.outFile.asParameter());
    }
    return ouput.join(" ");
  }
}
