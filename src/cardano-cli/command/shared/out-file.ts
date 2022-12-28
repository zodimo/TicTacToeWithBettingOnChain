import { CommandParameter } from "../command-parameter.js";

export class OutFile extends CommandParameter {
  constructor(private filename: string) {
      super();
  }
  asParameter(): string {
    return `--out-file ${this.filename}`;
  }
}
