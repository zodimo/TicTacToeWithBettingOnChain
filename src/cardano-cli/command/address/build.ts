/*
Usage: cardano-cli address build 
            ( --payment-verification-key STRING
            | --payment-verification-key-file FILE
            | --payment-script-file FILE
            )
            [ --stake-verification-key STRING
            | --stake-verification-key-file FILE
            | --stake-script-file FILE
            ]
            (--mainnet | --testnet-magic NATURAL)
            [--out-file FILE]
*/

import { Command } from "../command";

export class Build extends Command {
  constructor(private commandPrefix: string) {
    super();
  }
  
  getCommand(): string {
    throw new Error("Method not implemented.");
  }
}
