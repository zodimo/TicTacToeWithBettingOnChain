import { Command } from "../command.js";
import { OutFile } from "../shared/out-file.js";
import { StakeComponent } from "./stake-component.js";

/*
Usage: cardano-cli stake-address registration-certificate 
            ( --stake-verification-key STRING
            | --stake-verification-key-file FILE
            | --stake-script-file FILE
            )
            --out-file FILE
*/

export class RegistrationCertificate extends Command {
  private stakeComponent?: StakeComponent;
  private outFile?: OutFile;

  constructor(private commandPrefix: string) {
    super();
  }

  withStakeComponent(
    stakeComponent: StakeComponent
  ): RegistrationCertificate {
    this.stakeComponent = stakeComponent;
    return this;
  }

  withOutFile(outFile: OutFile): RegistrationCertificate {
    this.outFile = outFile;
    return this;
  }

  getCommand(): string {
    let ouput: string[] = [this.commandPrefix, "registration-certificate"];

    if (this.stakeComponent) {
      ouput.push(this.stakeComponent.asParameter());
    }

    if (this.outFile) {
      ouput.push(this.outFile.asParameter());
    }
    return ouput.join(" ");
  }
}
