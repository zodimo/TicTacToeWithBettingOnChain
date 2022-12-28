import { config } from "../../../cardano-cli-config.js";
import { Network } from "../network.js";
import { NodeMode } from "../node-mode.js";
import { Command } from "../command.js";
import { OutFile } from "../shared/out-file.js";

/*
Usage: cardano-cli query protocol-parameters 
            [ --shelley-mode
            | --byron-mode [--epoch-slots NATURAL]
            | --cardano-mode [--epoch-slots NATURAL]
            ]
            (--mainnet | --testnet-magic NATURAL)
            [--out-file FILE]
*/
export class ProtocolParameters extends Command {
  private network?: Network;
  private nodeMode?: NodeMode;
  private outFile?: OutFile;
  constructor(private commandPrefix: string) {
    super();
    this.network = config.getNetwork();
    this.nodeMode = config.getNodeMode();
  }

  withNetwork(network: Network): ProtocolParameters {
    this.network = network;
    return this;
  }

  withNodeMode(nodeMode: NodeMode): ProtocolParameters {
    this.nodeMode = nodeMode;
    return this;
  }

  withOutFile(outFile: OutFile): ProtocolParameters {
    this.outFile = outFile;
    return this;
  }
  getCommand(): string {
    let ouput: string[] = [this.commandPrefix, "protocol-parameters"];
    if (this.nodeMode) {
      ouput.push(this.nodeMode.asParameter());
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
