import { Command } from "../command.js";
import { Network } from "../network.js";
import { OutFile } from "../shared/out-file.js";

/*
Usage: cardano-cli address build-script --script-file FILE
            (--mainnet | --testnet-magic NATURAL)
            [--out-file FILE]
*/

export class BuildScript extends Command {
  private scriptFile?: string;
  private network?: Network;
  private outFile?: OutFile;
  constructor(private commandPrefix: string) {
    super();
  }

  withScriptFile(scriptFile: string): BuildScript {
    this.scriptFile = scriptFile;
    return this;
  }
  withNetwork(network: Network): BuildScript {
    this.network = network;
    return this;
  }

  withOutFile(outFile: OutFile): BuildScript {
    this.outFile = outFile;
    return this;
  }

  getCommand(): string {
    let ouput: string[] = [this.commandPrefix, "build-script"];

    if (this.scriptFile) {
      ouput.push(`--script-file ${this.scriptFile}`);
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
