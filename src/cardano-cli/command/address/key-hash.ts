/*
Usage: cardano-cli address key-hash 
            ( --payment-verification-key STRING
            | --payment-verification-key-file FILE
            )
            [--out-file FILE]
*/

import { Command } from "../command";

export class KeyHash extends Command {
  constructor(private commandPrefix: string) {
    super();
  }

  getCommand(): string {
    throw new Error("Method not implemented.");
  }
}
