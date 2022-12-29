import { config } from "../../../cardano-cli-config.js";
import { Command } from "../command.js";
import { Network } from "../network.js";
import { OutFile } from "../shared/out-file.js";
import { StakeComponent } from "./stake-component.js";

/*
Usage: cardano-cli stake-address build 
            ( --stake-verification-key STRING
            | --stake-verification-key-file FILE
            | --stake-script-file FILE
            )
            (--mainnet | --testnet-magic NATURAL)
            [--out-file FILE]
*/

export class Build extends Command {
  private stakeComponent?: StakeComponent;
  private network?: Network;
  private outFile?: OutFile;
  constructor(private commandPrefix: string) {
    super();
    this.network = config.getNetwork();
  }

  withStakeComponent(stakeComponent: StakeComponent): Build {
    this.stakeComponent = stakeComponent;
    return this;
  }

  withNetwork(network: Network): Build {
    this.network = network;
    return this;
  }

  withOutFile(outFile: OutFile): Build {
    this.outFile = outFile;
    return this;
  }

  getCommand(): string {
    let ouput: string[] = [this.commandPrefix, "build"];

    if (this.stakeComponent) {
      ouput.push(this.stakeComponent.asParameter());
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
