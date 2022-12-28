// --fee LOVELACE           The fee amount in Lovelace.

import { CommandParameter } from "../command-parameter.js";

export class Fee extends CommandParameter {
  constructor(private valueInLovelace: number) {
    super();
  }
  asParameter(): string {
    return `--fee ${this.valueInLovelace}`;
  }
}
