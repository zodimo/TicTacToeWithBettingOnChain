import { StackValue } from "./utxo.js";
export class Wallet {
  constructor(
    public readonly name: string,
    public readonly paymentAddr: string,
    public readonly stakingAddr: string | null
  ) {}
}
