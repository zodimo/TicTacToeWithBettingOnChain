import { Fee } from "../command/shared/fee.js";
import { TxInParameter } from "../command/transaction/build-raw/tx-in.js";
import { TxOutParameter } from "../command/transaction/build-raw/tx-out.js";

export class TransactionBuildRawOptions {
  private txIns: TxInParameter[];
  private txOuts: TxOutParameter[];
  private fee: Fee;
  constructor(txIns?: TxInParameter[], txOuts?: TxOutParameter[], fee?: Fee) {
    if (txIns) {
      this.txIns = txIns;
    } else {
      this.txIns = [];
    }

    if (txOuts) {
      this.txOuts = txOuts;
    } else {
      this.txOuts = [];
    }
    if (fee) {
      this.fee = fee;
    } else {
      this.fee = new Fee(0);
    }
  }

  withTxIn(txIn: TxInParameter): TransactionBuildRawOptions {
    this.txIns.push(txIn);
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
