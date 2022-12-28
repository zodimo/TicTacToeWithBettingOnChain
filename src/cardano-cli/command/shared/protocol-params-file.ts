// --protocol-params-file FILE
//                            Filepath of the JSON-encoded protocol parameters file

import { CommandParameter } from "../command-parameter.js";

export class ProtocolParamsFile extends CommandParameter {
  constructor(private filename: string) {
    super();
  }
  asParameter(): string {
    return `--protocol-params-file ${this.filename}`;
  }
}
