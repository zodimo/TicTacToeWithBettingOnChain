export class NodeMode {
  private constructor(private mode: string) {}

  static shelley(): NodeMode {
    return new NodeMode("shelley");
  }

  static byron(): NodeMode {
    return new NodeMode("byron");
  }
  static cardano(): NodeMode {
    return new NodeMode("cardano");
  }
  toString(): string {
    return this.mode;
  }

  asParameter(): string {
    return `--${this.mode}-mode`;
  }
}
