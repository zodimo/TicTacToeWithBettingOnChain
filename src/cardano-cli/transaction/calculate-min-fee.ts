
export class TransactionCalculateMinFeeOptions {
    constructor(
      public readonly txBodyFile: string,
      public readonly txInCount: number,
      public readonly txOutCount: number,
      public readonly witnessCount: number,
      public readonly byronWitnessCount: number = 0
    ) {}
  }