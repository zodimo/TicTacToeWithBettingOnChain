import cardanocliJs from "./previewCardanoCliJs";
import { TxIn as TxInInterface } from "cardanocli-js";
import { TxOut as TxOutInterface } from "cardanocli-js";
import { Utxo } from "cardanocli-js";

export class TxIn implements TxInInterface {
  constructor(
    public readonly txHash: string,
    public readonly txId: number,
    public readonly script?: any,
    public readonly datum?: any,
    public readonly redeemer?: any,
    public readonly executionUnits: any = null
  ) {}

  static fromUtxo(utxo: Utxo) {
    return new TxIn(utxo.txHash, utxo.txId);
  }
}

export class TxOut implements TxOutInterface {
  constructor(
    public readonly address: string,
    public readonly value: any,
    public readonly datumHash?: string
  ) {}
}
