import { config } from "../../../cardano-cli-config.js";
import { Network } from "../network.js";
import { NodeMode } from "../node-mode.js";
import { Command } from "../command.js";
import { OutFile } from "../shared/out-file.js";

/*
Usage: cardano-cli query tip 
            [ --shelley-mode
            | --byron-mode [--epoch-slots NATURAL]
            | --cardano-mode [--epoch-slots NATURAL]
            ]
            (--mainnet | --testnet-magic NATURAL)
            [--out-file FILE]
*/
export class Tip extends Command{
  private network: Network;
  private nodeMode: NodeMode;
  private outFile?: OutFile;
  constructor(private commandPrefix: string) {
      super();
    this.network = config.getNetwork();
    this.nodeMode = config.getNodeMode();
  }

  withNetwork(network: Network): Tip {
    this.network = network;
    return this;
  }

  withNodeMode(nodeMode: NodeMode): Tip {
    this.nodeMode = nodeMode;
    return this;
  }

  withOutFile(outFile: OutFile): Tip {
    this.outFile = outFile;
    return this;
  }
  getCommand(): string {
    let ouput: string[] = [this.commandPrefix, "tip"];
    if (this.nodeMode) {
      ouput.push(this.nodeMode.asParameter());
    }
    ouput.push(this.network.asParameter());
    if (this.outFile) {
      ouput.push(this.outFile.asParameter());
    }
    return ouput.join(" ");
  }
}
