import { CommandParameter } from "../command-parameter.js";
import { Command } from "../command.js";
import { OutFile } from "../shared/out-file.js";

/*
Usage: cardano-cli stake-address key-hash 
            ( --stake-verification-key STRING
            | --stake-verification-key-file FILE
            )
            [--out-file FILE]
*/
export class StakeVerificationKey extends CommandParameter {
  private constructor(private paramKey: string, private paramValue: string) {
    super();
  }

  static value(value: string): StakeVerificationKey {
    //--stake-verification-key STRING
    const param = "stake-verification-key";
    return new StakeVerificationKey(param, value);
  }
  static file(value: string): StakeVerificationKey {
    // --stake-verification-key-file FILE
    const param = "stake-verification-key-file";
    return new StakeVerificationKey(param, value);
  }

  asParameter(): string {
    return `--${this.paramKey} ${this.paramValue}`;
  }
}

export class KeyHash extends Command {
  private stakeVerificationKey?: StakeVerificationKey;
  private outFile?: OutFile;

  constructor(private commandPrefix: string) {
    super();
  }

  withStakeVerificationKey(
    stakeVerificationKey: StakeVerificationKey
  ): KeyHash {
    this.stakeVerificationKey = stakeVerificationKey;
    return this;
  }

  withOutFile(outFile: OutFile): KeyHash {
    this.outFile = outFile;
    return this;
  }

  getCommand(): string {
    let ouput: string[] = [this.commandPrefix, "key-hash"];
    if (this.stakeVerificationKey) {
      ouput.push(this.stakeVerificationKey.asParameter());
    }
    if (this.outFile) {
      ouput.push(this.outFile.asParameter());
    }
    return ouput.join(" ");
  }
}
