export class UtxoId {
    constructor(
      public readonly transactionHash: string,
      public readonly outputIndex: number
    ) {}
  
    static fromString(value: string): UtxoId {
      const regex = new RegExp(
        /(?<transactionHash>[a-f0-9]+)#(?<outputIndex>\d+)/,
        "g"
      );
      const matches = regex.exec(value);
      let transactionHash: string | null = null;
      let outputIndex: string | null = null;
  
      if (!matches) {
        throw new Error(`Invalid UtxoId format.`);
      }
  
      if (!matches.groups) {
        throw new Error(`Invalid UtxoId format.`);
      }
  
      transactionHash = matches.groups.transactionHash;
      outputIndex = matches.groups.outputIndex;
  
      if (!transactionHash || !outputIndex) {
        throw new Error(`Invalid UtxoId format.`);
      }
  
      return new UtxoId(transactionHash, +outputIndex);
    }
  
    equals(utxoId: UtxoId): boolean {
      return this.toString() == utxoId.toString();
    }
  
    toString(): string {
      return `${this.transactionHash}#${this.outputIndex}`;
    }
  }