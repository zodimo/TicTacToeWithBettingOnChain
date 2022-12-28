import { config } from "../../../cardano-cli-config.js";
import { Command } from "../command.js";
import { Network } from "../network.js";
import { NodeMode } from "../node-mode.js";
/*
Usage: cardano-cli transaction submit 
            [ --shelley-mode
            | --byron-mode [--epoch-slots NATURAL]
            | --cardano-mode [--epoch-slots NATURAL]
            ]
            (--mainnet | --testnet-magic NATURAL)
            --tx-file FILE
*/
export class Submit extends Command {
  private network?: Network;
  private nodeMode?: NodeMode;
  private txFile?: string;

  constructor(private commandPrefix: string) {
    super();
    this.network = config.getNetwork();
    this.nodeMode = config.getNodeMode();
  }

  withNetwork(network: Network): Submit {
    this.network = network;
    return this;
  }
  withNodeMode(nodeMode: NodeMode): Submit {
    this.nodeMode = nodeMode;
    return this;
  }
  withTxFile(filename: string): Submit {
    this.txFile = filename;
    return this;
  }

  getCommand(): string {
    let output: string[] = [this.commandPrefix, "submit"];
    if (this.nodeMode) {
      output.push(this.nodeMode.asParameter());
    }
    if (this.network) {
      output.push(this.network.asParameter());
    }
    if (this.txFile) {
      output.push(`--tx-file ${this.txFile}`);
    }
    return output.join(" ");
  }
}
