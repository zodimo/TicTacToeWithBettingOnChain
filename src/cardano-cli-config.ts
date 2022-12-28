import os from "os";
import path from "path";
import { Era } from "./cardano-cli/era.js";
import { Network } from "./cardano-cli/command/network.js";
import { NodeMode } from "./cardano-cli/command/node-mode.js";

class Config {
  private cardanoCliBinPath: string;
  private workingDirectory: string;
  private network: Network;
  private era: Era;
  private nodeMode: NodeMode;
  private debug: boolean;

  constructor() {
    this.cardanoCliBinPath = "cardano-cli";
    this.workingDirectory = path.join(
      os.homedir(),
      "CardanoProjects",
      "testnet"
    );
    this.network = Network.testnetMagic(2);
    this.era = Era.babbage();
    this.nodeMode = NodeMode.cardano();
    this.debug = true;
  }

  getCardanoCliBinPath(): string {
    return this.cardanoCliBinPath;
  }

  getWorkingDirectory(): string {
    return this.workingDirectory;
  }

  getNetwork(): Network {
    return this.network;
  }

  getEra(): Era {
    return this.era;
  }
  getNodeMode(): NodeMode {
    return this.nodeMode;
  }

  getDebug(): boolean {
    return this.debug;
  }
}

//load dot env here..
export const config = new Config();
