import { execSync } from "child_process";
import fs from "fs";
import { AddressKeys, Wallet, WalletKeys } from "./wallet.js";
import {
  StackValue,
  Utxo,
  UtxoNativeAsset,
  UtxoStack,
  UtxoValue,
} from "./utxo.js";

import {
  TransactionBuildRawOptions,
  TransactionCalculateMinFeeOptions,
  TransactionSignOptions,
  TransactionSubmitOptions,
  TxIdOptions,
  TxIdTx,
} from "./transaction.js";
import { stringify } from "querystring";
import { Address } from "@emurgo/cardano-serialization-lib-nodejs";
import { Network } from "./network.js";

export interface CardanoCliOptionsInterface {
  shelleyGenesisPath: string;
  cliPath: string | null;
  dir: string;
  era: string;
  network: Network;
  debug: boolean | null;
}

export class CardanoCliOptions implements CardanoCliOptionsInterface {
  constructor(
    public readonly shelleyGenesisPath: string,
    public readonly dir: string,
    public readonly era: string,
    public readonly network: Network,
    public readonly cliPath: string | null = null,
    public readonly debug: boolean | null = null
  ) {}
}

export class CardanoCli {
  network: Network;
  era: string;
  dir: string;
  cliPath: string;
  shelleyGenesis: string;
  protocolParametersFile: string;
  debug: boolean;

  constructor(options: CardanoCliOptionsInterface) {
    //defaults
    this.dir = ".";
    this.cliPath = "cardano-cli";
    this.debug = true;

    options.debug && (this.debug = options.debug);

    this.shelleyGenesis = JSON.parse(
      this.runCommand(`cat ${options.shelleyGenesisPath}`)
    );

    this.era = "--" + options.era + "-era";
    this.network = options.network;
    options.dir && (this.dir = options.dir);
    options.cliPath && (this.cliPath = options.cliPath);


    this.protocolParametersFile = `${this.dir}/tmp/protocolParams.json`;

    this.runCommand(`mkdir -p ${this.dir}/tmp`);
    this.ensureProtocolParametersPathExist();
  }

  private runCommand(command:string):string{

    const formattedCommand=command.replace(/\s+/g, " ");
    if(this.debug){
      console.log("DEBUG: " + formattedCommand)
    }
    return execSync(formattedCommand).toString();
  }

  queryTip() {
    return JSON.parse(
      this.runCommand(`${this.cliPath} query tip \
        ${this.network.asParameter()} \
        --cardano-mode`)
    );
  }

  wallet(account: string): Wallet {
    const paymentAddrFile = `${this.dir}/priv/wallet/${account}/${account}.payment.addr`;
    const paymentAddrSigningKeyFile = `${this.dir}/priv/wallet/${account}/${account}.payment.skey`;
    const paymentAddrVerificationKeyFile = `${this.dir}/priv/wallet/${account}/${account}.payment.vkey`;

    if (!fs.existsSync(paymentAddrFile)) {
      throw new Error(`Payment Address for ${account} does not exist.`);
    }

    if (!fs.existsSync(paymentAddrVerificationKeyFile)) {
      throw new Error(
        `Payment Verification Key for ${account} does not exist.`
      );
    }

    if (!fs.existsSync(paymentAddrSigningKeyFile)) {
      throw new Error(`Payment Signing Key for ${account} does not exist.`);
    }

    const paymentkeys = new AddressKeys(
      paymentAddrVerificationKeyFile,
      paymentAddrSigningKeyFile
    );

    const paymentAddr = fs.readFileSync(paymentAddrFile).toString();

    const stakingAddrFile = `${this.dir}/priv/wallet/${account}/${account}.stake.addr`;

    let stakingAddr: string | null = null;
    if (fs.existsSync(stakingAddrFile)) {
      stakingAddr = fs
        .readFileSync(
          `${this.dir}/priv/wallet/${account}/${account}.stake.addr`
        )
        .toString();
    }

    return new Wallet(
      account,
      paymentAddr,
      stakingAddr,
      new WalletKeys(paymentkeys)
    );
  }

  getUtxoStackFor(paymentAddr: string): UtxoStack {
    const utxos = this.queryUtxo(paymentAddr);
    return new UtxoStack(utxos);
  }

  queryUtxo(address: string) {
    const UID = Math.random().toString(36).slice(2, 9);
    const utxosTempFile = `${this.dir}/tmp/utxo_${UID}.json`;
    this.runCommand(`${this.cliPath} query utxo \
    ${this.network.asParameter()} \
    --address ${address} \
    --cardano-mode \
    --out-file ${utxosTempFile}
    `);

    const utxosRaw = JSON.parse(this.runCommand(`cat ${utxosTempFile}`));
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

  transactionBuildRaw(options: TransactionBuildRawOptions): string {
    let UID = Math.random().toString(36).slice(2, 9);

    // building txIn string
    let txInString: string = "";
    options.txIn.forEach((value) => {
      txInString += ` ${value.asParameter()}`;
    });

    // building txOut string
    let txOutString: string = "";
    options.txOut.forEach((value) => {
      txOutString += ` ${value.asParameter()}`;
    });

    const command = `${this.cliPath} transaction build-raw \
    ${this.era} \
    ${txInString} \
    ${txOutString} \
    --fee ${options.fee ? options.fee : 0} \
    --out-file ${this.dir}/tmp/tx_${UID}.raw \
    --protocol-params-file ${this.protocolParametersFile}`;

    this.runCommand(command);

    return `${this.dir}/tmp/tx_${UID}.raw`;
  }

  queryProtocolParameters() {
    return JSON.parse(
      this.runCommand(`cat ${this.dir}/tmp/protocolParams.json`)
    );
  }

  writeProtocolParametersFile(): void {
    this.runCommand(`${this.cliPath} query protocol-parameters \
    ${this.network.asParameter()} \
    --cardano-mode \
    --out-file ${this.dir}/tmp/protocolParams.json
`);
  }

  ensureProtocolParametersPathExist(): void {
    if (!fs.existsSync(this.protocolParametersFile)) {
      this.writeProtocolParametersFile();
    }
  }

  toLovelace(ada: number): number {
    return ada * 1000000;
  }

  toAda(lovelace: number): number {
    return lovelace / 1000000;
  }

  transactionCalculateMinFee(
    options: TransactionCalculateMinFeeOptions
  ): number {
    return parseInt(
      this.runCommand(`${this.cliPath} transaction calculate-min-fee \
                --tx-body-file ${options.txBodyFile} \
                --tx-in-count ${options.txInCount} \
                --tx-out-count ${options.txOutCount} \
                ${this.network.asParameter()} \
                --witness-count ${options.witnessCount} \
                --protocol-params-file ${this.protocolParametersFile}`)
        .replace(/\s+/g, " ")
        .split(" ")[0]
    );
  }

  transactionSign(options: TransactionSignOptions): string {
    const UID = Math.random().toString(36).slice(2, 9);
    this.runCommand(`${this.cliPath} transaction sign \
        ${options.txToSign.asParameter()} \
        ${this.network.asParameter()} \
        ${options.signingKeyFiles.asParameter()} \
        ${options.address.asParameter()} \
        --out-file ${this.dir}/tmp/tx_${UID}.signed`);
    return `${this.dir}/tmp/tx_${UID}.signed`;
  }

  transactionSubmit(options: TransactionSubmitOptions) {
    let UID = Math.random().toString(36).slice(2, 9);
    this.runCommand(
      `${
        this.cliPath
      } transaction submit ${this.network.asParameter()} --tx-file ${
        options.txFile
      }`
    );

    return this.transactionTxid(new TxIdOptions(TxIdTx.file(options.txFile)));
  }

  transactionTxid(options: TxIdOptions): string {
    return this.runCommand(
      `${this.cliPath} transaction txid ${options.tx.asParameter()}`
    )
      .toString()
      .trim();
  }
}
