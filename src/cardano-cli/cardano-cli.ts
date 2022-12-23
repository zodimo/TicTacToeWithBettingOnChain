import { execSync } from "child_process";
import fs from "fs";
import { Wallet } from "./wallet.js";
import { Utxo, UtxoNativeAsset, UtxoValue } from "./utxo.js";

export interface ConstructorOptionsInterface {
  shelleyGenesisPath: string;
  cliPath?: string;
  dir: string;
  era: string;
  network: string;
}

export class ConstructorOptions implements ConstructorOptionsInterface {
  constructor(
    public readonly shelleyGenesisPath: string,
    public readonly dir: string,
    public readonly era: string,
    public readonly network: string,
    public readonly cliPath?: string
  ) {}
}

export class CardanoCli {
  network: string;
  era: string;
  dir: string;
  cliPath: string;
  shelleyGenesis: string;

  constructor(options: ConstructorOptionsInterface) {
    //defaults
    this.dir = ".";
    this.cliPath = "cardano-cli";

    this.shelleyGenesis = JSON.parse(
      execSync(`cat ${options.shelleyGenesisPath}`).toString()
    );

    this.era = "--" + options.era + "-era";
    this.network = options.network;
    options.dir && (this.dir = options.dir);
    options.cliPath && (this.cliPath = options.cliPath);
  }

  queryTip() {
    return JSON.parse(
      execSync(`${this.cliPath} query tip \
        --${this.network} \
        --cardano-mode
                        `).toString()
    );
  }

  wallet(account: string): Wallet {
    let stakingAddr = "No staking keys generated";

    this.assertPaymentAddressExists(account);

    const paymentAddr = fs
      .readFileSync(
        `${this.dir}/priv/wallet/${account}/${account}.payment.addr`
      )
      .toString();

    // stakingAddr = fs
    //   .readFileSync(`${this.dir}/priv/wallet/${account}/${account}.stake.addr`)
    //   .toString();

    return new Wallet(account, paymentAddr);
  }

  assertPaymentAddressExists(account: string) {
    const path = `${this.dir}/priv/wallet/${account}/${account}.payment.addr`;
    if (!fs.existsSync(path)) {
      throw new Error(`Payment Address for ${account} does not exist.`);
    }
  }

  queryUtxo(address: string) {
    const UID = Math.random().toString(36).slice(2, 9);
    const utxosTempFile = `${this.dir}/tmp/utxo_${UID}.json`;
    execSync(`${this.cliPath} query utxo \
    --${this.network} \
    --address ${address} \
    --cardano-mode \
    --out-file ${utxosTempFile}
    `);

    const utxosRaw = JSON.parse(execSync(`cat ${utxosTempFile}`).toString());
    // remove temp file.
    fs.rmSync(utxosTempFile);

    let utxoList: Utxo[] = [];

    interface UtxoRawAssetInterface {
      [key: string]: number;
    }

    interface UtxoRawAssetsInterface {
      [key: string]: UtxoRawAssetInterface;
    }

    Object.keys(utxosRaw).forEach((utxo: string) => {
      // keeping the types on utxoBody loose to make it work.
      const utxoBody = utxosRaw[utxo];
      const {
        lovelace,
        ...utxoRawAssets
      }: { lovelace: number; utxoRawAssets: UtxoRawAssetInterface } =
        utxoBody["value"];
      let utxoNativeAssets: UtxoNativeAsset[] = [];

      Object.entries(utxoRawAssets).forEach(([policyId, utxoRawAsset]) => {
        Object.entries(utxoRawAsset).forEach(([assetName, quantity]) => {
          utxoNativeAssets.push(
            new UtxoNativeAsset(policyId, assetName, quantity)
          );
        });
      });

      const utxoValue = new UtxoValue(lovelace, utxoNativeAssets);
      utxoList.push(
        new Utxo(
          utxo,
          utxoBody.address,
          utxoValue,
          utxoBody.datum,
          utxoBody.datumhash,
          utxoBody.inlineDatum,
          utxoBody.referenceScript
        )
      );
    });

    return utxoList;
  }
}
