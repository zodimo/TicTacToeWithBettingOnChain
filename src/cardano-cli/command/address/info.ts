import { Command } from "../command.js";
import { OutFile } from "../shared/out-file.js";

/*
Usage: cardano-cli address info --address ADDRESS [--out-file FILE]
*/

export class Info extends Command {
  private address?: string;
  private outFile?: OutFile;
  constructor(private commandPrefix: string) {
    super();
  }
  withAddress(address: string): Info {
    this.address = address;
    return this;
  }
  withOutFile(outFile: OutFile): Info {
    this.outFile = outFile;
    return this;
  }
  getCommand(): string {
    let ouput: string[] = [this.commandPrefix, "info"];

    if (this.address) {
      ouput.push(`--address ${this.address}`);
    }

    if (this.outFile) {
      ouput.push(this.outFile.asParameter());
    }
    return ouput.join(" ");
  }
}
