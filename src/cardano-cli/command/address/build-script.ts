/*
Usage: cardano-cli address build-script --script-file FILE
            (--mainnet | --testnet-magic NATURAL)
            [--out-file FILE]
*/

import { Command } from "../command";

export class BuildScript extends Command {
  constructor(private commandPrefix: string) {
    super();
  }
  
  getCommand(): string {
    throw new Error("Method not implemented.");
  }
}
