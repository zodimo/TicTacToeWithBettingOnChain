import { CommandParameter } from "../../command-parameter";

export class WholeUtxo extends CommandParameter {
  asParameter(): string {
    return "--whole-utxo";
  }
}
