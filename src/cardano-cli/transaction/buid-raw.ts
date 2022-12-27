import { TxInParameter } from "./build-raw/tx-in.js";
import { TxOutParameter } from "./build-raw/tx-out.js";


export class TransactionBuildRawOptions {
  constructor(
    public txIn: TxInParameter[],
    public txOut: TxOutParameter[],
    public fee: number = 0
  ) {}
}
