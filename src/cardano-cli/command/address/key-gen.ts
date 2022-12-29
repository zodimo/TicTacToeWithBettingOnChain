/*
Usage: cardano-cli address key-gen [--normal-key | --extended-key | --byron-key]
            --verification-key-file FILE
            --signing-key-file FILE
*/

import { Command } from "../command";

export class KeyGen extends Command {
  constructor(private commandPrefix: string) {
    super();
  }

  getCommand(): string {
    throw new Error("Method not implemented.");
  }
}
