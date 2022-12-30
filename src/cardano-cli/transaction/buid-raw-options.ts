import { Fee } from "../command/shared/fee.js";
import { TxInParameter } from "../command/transaction/build-raw/tx-in.js";
import { TxOutParameter } from "../command/transaction/build-raw/tx-out.js";

export class TransactionBuildRawOptions {
  private txIns: TxInParameter[];
  private txOuts: TxOutParameter[];
  private fee: Fee;
  constructor() {
    this.txIns = [];
    this.txOuts = [];
    this.fee = new Fee(0);
  }

  withTxIns(txIns: TxInParameter[]): TransactionBuildRawOptions {
    this.txIns = txIns;
    return this;
  }
  withTxIn(txIn: TxInParameter): TransactionBuildRawOptions {
    this.txIns.push(txIn);
    return this;
  }
  withTxOuts(txOuts: TxOutParameter[]): TransactionBuildRawOptions {
    this.txOuts = txOuts;
    return this;
  }
  withTxOut(txOut: TxOutParameter): TransactionBuildRawOptions {
    this.txOuts.push(txOut);
    return this;
  }

  withFee(fee: Fee): TransactionBuildRawOptions {
    this.fee = fee;
    return this;
  }

  getTxIns(): TxInParameter[] {
    return this.txIns;
  }
  getTxOuts(): TxOutParameter[] {
    return this.txOuts;
  }
  getFee(): Fee {
    return this.fee;
  }
}
