import { Command } from "../command.js";
import { OutFile } from "../shared/out-file.js";
import { StakeComponent } from "./stake-component.js";
import { StakePoolComponent } from "./stake-pool-component.js";
/*
Usage: cardano-cli stake-address delegation-certificate 
            ( --stake-verification-key STRING
            | --stake-verification-key-file FILE
            | --stake-script-file FILE
            )
            ( --stake-pool-verification-key STRING
            | --cold-verification-key-file FILE
            | --stake-pool-id STAKE-POOL-ID
            )
            --out-file FILE
*/

export class DelegationCertificate extends Command {
  private stakeComponent?: StakeComponent;
  private stakePoolComponent?: StakePoolComponent;
  private outFile?: OutFile;

  constructor(private commandPrefix: string) {
    super();
  }
  withStakeComponent(stakeComponent: StakeComponent): DelegationCertificate {
    this.stakeComponent = stakeComponent;
    return this;
  }
  withStakePoolComponent(
    stakePoolComponent: StakePoolComponent
  ): DelegationCertificate {
    this.stakePoolComponent = stakePoolComponent;
    return this;
  }

  withOutFile(outFile: OutFile): DelegationCertificate {
    this.outFile = outFile;
    return this;
  }

  getCommand(): string {
    let ouput: string[] = [this.commandPrefix, "delegation-certificate"];

    if (this.stakeComponent) {
      ouput.push(this.stakeComponent.asParameter());
    }
    if (this.stakePoolComponent) {
      ouput.push(this.stakePoolComponent.asParameter());
    }

    if (this.outFile) {
      ouput.push(this.outFile.asParameter());
    }
    return ouput.join(" ");
  }
}
