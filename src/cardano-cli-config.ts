import os from "os";
import path from "path";
import { Era } from "./cardano-cli/command/era.js";
import { Network } from "./cardano-cli/command/network.js";
import { NodeMode } from "./cardano-cli/command/node-mode.js";

import dotenv from "dotenv";

class Config {
  private cardanoCliBinPath: string;
  private workingDirectory: string;
  private network: Network;
  private era: Era;
  private nodeMode: NodeMode;
  private debug: boolean;

  constructor(cardanoCliBin?: string, workingDirectory?: string, testnetMagicNumber?: number, debug?: boolean) {
    this.cardanoCliBinPath = cardanoCliBin ?? "cardano-cli";
    // Jaco default
    // this.workingDirectory = workingDirectory ?? path.join(os.homedir(), "CardanoProjects", "testnet");
    this.workingDirectory = workingDirectory ?? "./workdir";
    this.network = Network.testnetMagic(testnetMagicNumber ?? 2);
    this.era = Era.babbage();
    this.nodeMode = NodeMode.cardano();
    this.debug = debug ?? true;
  }

  static createFromEnv(): Config {
    const cardanoCliBinPath = process.env.CARDANO_CLI_BIN_PATH ?? "cardano-cli";
    const workingDirectory = process.env.WORKING_DIRECTORY;
    const networkTestnetMagicNumber = process.env.NETWORK_TESTNET_MAGIC_NUMBER ?? 2;
    const debug = !!process.env.CARDANO_CLI_DEBUG ?? true;
    return new Config(cardanoCliBinPath, workingDirectory, +networkTestnetMagicNumber, debug);
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
dotenv.config();
export const config = Config.createFromEnv();

