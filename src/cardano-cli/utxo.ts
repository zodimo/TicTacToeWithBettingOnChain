export class UtxoNativeAsset {
  constructor(
    public readonly policyId: string,
    public readonly assetName: string,
    public readonly quantity: number
  ) {}
}

export class UtxoValue {
  constructor(
    public readonly lovelace: number,
    public readonly nativeAssets: UtxoNativeAsset[]
  ) {
    // { lovelace: 9389640398 }
  }
}

export class Utxo {
  constructor(
    public readonly id: string,
    public readonly address: string,
    public readonly value: UtxoValue,
    public readonly datum?: string,
    public readonly datumHash?: string,
    public readonly inlineDatum?: string,
    public readonly referenceScript?: string
  ) {}
}

export interface StackValue {
  [key: string]: number;
}

export class UtxoStack {
  utxos: Utxo[];
  value: StackValue;

  constructor(utxos: Utxo[]) {
    this.value = { lovelace: 0 };
    this.utxos = [];
    utxos.forEach((utxo) => {
      this.addUtxo(utxo);
    });
  }

  private addUtxo(utxo: Utxo): void {
    this.value["lovelace"] += utxo.value.lovelace;
    utxo.value.nativeAssets.forEach((nativeAsset) => {
      const assetId = `${nativeAsset.policyId}.${nativeAsset.assetName}`;
      if (this.value.hasOwnProperty(assetId)) {
        this.value[assetId] += nativeAsset.quantity;
      } else {
        this.value[assetId] = nativeAsset.quantity;
      }
    });

    this.utxos.push(utxo);
  }

  getValue(): StackValue {
    return this.value;
  }
}
