/*
Usage: cardano-cli address info --address ADDRESS [--out-file FILE]
*/

import { Command } from "../command";

export class Info extends Command {
  constructor(private commandPrefix: string) {
    super();
  }
  
  getCommand(): string {
    throw new Error("Method not implemented.");
  }
}
