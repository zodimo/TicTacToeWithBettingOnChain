import { execSync } from "child_process";
import fs from "fs";
import { AddressKeys, ScriptWallet, Wallet, WalletKeys } from "./wallet.js";
import {
  StackValue,
  Utxo,
  UtxoId,
  UtxoNativeAsset,
  UtxoStack,
  UtxoValue,
} from "./utxo.js";

import { Network } from "./network.js";
import { PaymentAddressBuildOptions } from "./address.js";
import { Era } from "./era.js";
import { NodeMode } from "./node-mode.js";
import { TransactionBuildOptions } from "./transaction-build.js";
import { TransactionBuildRawOptions } from "./transaction/buid-raw.js";
import { TransactionCalculateMinFeeOptions } from "./transaction/calculate-min-fee.js";
import { TransactionSignOptions } from "./transaction/sign.js";
import { TransactionSubmitOptions } from "./transaction/submit.js";
import { TxIdOptions, TxIdTx } from "./transaction/txid.js";

export interface CardanoCliOptionsInterface {
  shelleyGenesisPath: string;
  cliPath: string | null;
  dir: string;
  era: Era;
  network: Network;
  debug: boolean | null;
  nodeMode: NodeMode | null;
}

export class CardanoCliOptions implements CardanoCliOptionsInterface {
  constructor(
    public readonly shelleyGenesisPath: string,
    public readonly dir: string,
    public readonly era: Era,
    public readonly network: Network,
    public readonly cliPath: string | null = null,
    public readonly nodeMode: NodeMode | null = null,
    public debug: boolean | null = null
  ) {}
}

export class CardanoCli {
  network: Network;
  era: Era;
  dir: string;
  cliPath: string;
  shelleyGenesis: string;
  protocolParametersFile: string;
  debug: boolean;
  nodeMode: NodeMode;

  constructor(options: CardanoCliOptionsInterface) {
    //defaults
    this.dir = ".";
    this.cliPath = "cardano-cli";
    this.debug = true;
    this.nodeMode = NodeMode.cardano();

    options.debug !== null && (this.debug = options.debug);

    this.shelleyGenesis = JSON.parse(
      this.runCommand(`cat ${options.shelleyGenesisPath}`)
    );

    this.era = options.era;
    this.network = options.network;
    options.dir && (this.dir = options.dir);
    options.cliPath && (this.cliPath = options.cliPath);
    options.nodeMode && (this.nodeMode = options.nodeMode);

    this.protocolParametersFile = `${this.dir}/tmp/protocolParams.json`;

    this.ensureTempDirectoryExists();

    this.writeProtocolParametersFile();
    this.ensureProtocolParametersFileExist();
  }

  private ensureTempDirectoryExists(): void {
    const tempDirPath = `${this.dir}/tmp`;
    if (!fs.existsSync(tempDirPath)) this.runCommand(`mkdir -p ${tempDirPath}`);
  }

  private runCommand(command: string): string {
    const formattedCommand = command.replace(/\s+/g, " ");
    if (this.debug) {
      console.log("DEBUG: " + formattedCommand);
    }
    return execSync(formattedCommand).toString().trim();
  }

  queryTip() {
    return JSON.parse(
      this.runCommand(`${this.cliPath} query tip \
        ${this.network.asParameter()} \
        ${this.nodeMode.asParameter}`)
    );
  }

  scriptWallet(account: string): ScriptWallet {
    const paymentAddrFile =
      this.createPaymentAddressFileNameForAccount(account);
    if (!fs.existsSync(paymentAddrFile)) {
      throw new Error(`Payment Address for ${account} does not exist.`);
    }
    const paymentAddr = fs.readFileSync(paymentAddrFile).toString();

    return new ScriptWallet(account, paymentAddr);
  }

  wallet(account: string): Wallet {
    const paymentAddrFile =
      this.createPaymentAddressFileNameForAccount(account);
    const paymentAddrSigningKeyFile =
      this.createPaymentSKeyFileNameForAccount(account);
    const paymentAddrVerificationKeyFile =
      this.createPaymentVKeyFileNameForAccount(account);

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

    const stakingAddrFile =
      this.createStakingAddressFileNameForAccount(account);

    let stakingAddr: string | null = null;
    if (fs.existsSync(stakingAddrFile)) {
      stakingAddr = fs.readFileSync(stakingAddrFile).toString();
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
    ${this.nodeMode.asParameter()} \
    --out-file ${utxosTempFile}
    `);

    const utxosRaw = JSON.parse(this.runCommand(`cat ${utxosTempFile}`));

    if (!this.debug) {
      // remove temp file.
      fs.rmSync(utxosTempFile);
    }

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
          UtxoId.fromString(utxo),
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
    ${this.era.asParameter()} \
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
    ${this.nodeMode.asParameter()} \
    --out-file ${this.dir}/tmp/protocolParams.json
`);
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
      this.runCommand(
        `${this.cliPath} transaction calculate-min-fee \
                --tx-body-file ${options.txBodyFile} \
                --tx-in-count ${options.txInCount} \
                --tx-out-count ${options.txOutCount} \
                ${this.network.asParameter()} \
                --witness-count ${options.witnessCount} \
                --protocol-params-file ${this.protocolParametersFile}`
      )
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

  transactionPolicyid(scriptFile: string) {
    return this.runCommand(
      `${this.cliPath} transaction policyid --script-file ${scriptFile}`
    );
  }

  /**
   * Wallet
   */
  createWalletPathForAccount(account: string): string {
    return `${this.dir}/priv/wallet/${account}`;
  }

  /**
   * Payment Address
   */

  createPaymentSKeyFileNameForAccount(account: string): string {
    const walletPath = this.createWalletPathForAccount(account);
    return `${walletPath}/${account}.payment.skey`;
  }

  createPaymentVKeyFileNameForAccount(account: string): string {
    const walletPath = this.createWalletPathForAccount(account);
    return `${walletPath}/${account}.payment.vkey`;
  }

  createPaymentAddressFileNameForAccount(account: string): string {
    const walletPath = this.createWalletPathForAccount(account);
    return `${walletPath}/${account}.payment.addr`;
  }

  paymentAddressKeyGen(account: string): AddressKeys {
    let vkey = this.createPaymentVKeyFileNameForAccount(account);
    let skey = this.createPaymentSKeyFileNameForAccount(account);

    this.ensureKeysDoNoAlreadyExist(vkey, skey);

    this.ensurePathExists(this.createWalletPathForAccount(account));
    this.runCommand(`${this.cliPath} address key-gen \
                        --verification-key-file ${vkey} \
                        --signing-key-file ${skey}
                    `);
    return new AddressKeys(vkey, skey);
  }

  paymentAddressBuild(account: string, options: PaymentAddressBuildOptions) {
    const paymentAddressFileName =
      this.createPaymentAddressFileNameForAccount(account);
    this.ensurePathExists(this.createWalletPathForAccount(account));

    let stakingAddressString: string = "";
    if (options.stakingVerification) {
      stakingAddressString = options.stakingVerification.asParameter();
    }

    this.runCommand(`${this.cliPath} address build \
                    ${options.paymentVerification.asParameter()} \
                    ${stakingAddressString} 
                    --out-file ${paymentAddressFileName} \
                    ${this.network.asParameter()}
                `);
    return paymentAddressFileName;
  }

  /**
   * Staking Address
   */

  createStakingSKeyFileNameForAccount(account: string): string {
    const walletPath = this.createWalletPathForAccount(account);
    return `${walletPath}/${account}.stake.skey`;
  }

  createStakingVKeyFileNameForAccount(account: string): string {
    const walletPath = this.createWalletPathForAccount(account);
    return `${walletPath}/${account}.stake.vkey`;
  }

  createStakingAddressFileNameForAccount(account: string): string {
    const walletPath = this.createWalletPathForAccount(account);
    return `${walletPath}/${account}.stake.addr`;
  }

  stakeAddressKeyGen(account: string): AddressKeys {
    const vkey = this.createStakingVKeyFileNameForAccount(account);
    const skey = this.createStakingSKeyFileNameForAccount(account);

    this.ensureKeysDoNoAlreadyExist(vkey, skey);
    this.ensurePathExists(this.createWalletPathForAccount(account));
    this.runCommand(`${this.cliPath} stake-address key-gen \
                        --verification-key-file ${vkey} \
                        --signing-key-file ${skey}
                    `);
    return new AddressKeys(vkey, skey);
  }

  stakeAddressBuild(account: string): string {
    this.ensurePathExists(this.createWalletPathForAccount(account));
    const stakingVKeyFileName =
      this.createStakingVKeyFileNameForAccount(account);
    const stakingAddrFileName =
      this.createStakingVKeyFileNameForAccount(account);
    this.runCommand(`${this.cliPath} stake-address build \
                        --staking-verification-key-file ${stakingVKeyFileName} \
                        --out-file ${stakingAddrFileName} \
                        ${this.network.asParameter}
                    `);
    return stakingAddrFileName;
  }

  ensureKeysDoNoAlreadyExist(verificationKey: string, signingKey: string) {
    if (fs.existsSync(verificationKey)) {
      throw new Error(`Verification key already exists: ${verificationKey}`);
    }
    if (fs.existsSync(signingKey)) {
      throw new Error(`Signing key already exists: ${signingKey}`);
    }
  }

  ensurePathExists(path: string, createIfNotExists: boolean = true) {
    if (!fs.existsSync(path) && createIfNotExists) {
      fs.mkdirSync(path, { recursive: true });
    }

    if (!fs.existsSync(path)) {
      throw new Error(`Path does not exist: ${path}`);
    }
  }

  ensureProtocolParametersFileExist(createIfNotExists: boolean = true): void {
    if (!fs.existsSync(this.protocolParametersFile) && createIfNotExists) {
      this.writeProtocolParametersFile();
    }

    if (!fs.existsSync(this.protocolParametersFile)) {
      throw new Error(
        `Protocol Parameters File do not exist: ${this.protocolParametersFile}`
      );
    }
  }

  private sleep(seconds: number) {
    return this.runCommand(`sleep ${seconds}`);
  }

  waitForUtxoAtPaymentAddress(
    paymentAddress: string,
    utxoId: UtxoId,
    timoutInSeconds: number = 60
  ): void {
    let sleepCounter = 0;
    while (true) {
      if (!this.getUtxoStackFor(paymentAddress).hasUtxo(utxoId)) {
        if (this.debug) {
          console.log(`Waiting for TX: ${utxoId} [${sleepCounter}s]`);
        }
        this.sleep(1);
        sleepCounter++;
      } else {
        // utxo is there exit loop.
        break;
      }
      if (sleepCounter >= timoutInSeconds) {
        throw new Error(`Timeout exceeded, waiting for utxo.`);
      }
    }
  }

  // transactionBuild(options:TransactionBuildOptions) {

    
  //   let UID = Math.random().toString(36).slice(2, 9);
  //   const txInString = txInToString(this.dir, options.txIn);
  //   const txOutString = txOutToString(options.txOut);
  //   const txInCollateralString = options.txInCollateral
  //     ? txInToString(this.dir, options.txInCollateral, true)
  //     : "";
  //   const changeAddressString = options.changeAddress
  //     ? `--change-address ${options.changeAddress}`
  //     : "";
  //   const mintString = options.mint ? mintToString(this.dir, options.mint) : "";
  //   const withdrawals = options.withdrawals
  //     ? withdrawalToString(this.dir, options.withdrawals)
  //     : "";
  //   const certs = options.certs ? certToString(this.dir, options.certs) : "";
  //   const metadata = options.metadata
  //     ? "--metadata-json-file " +
  //       jsonToPath(this.dir, options.metadata, "metadata")
  //     : "";
  //   const auxScript = options.auxScript
  //     ? auxScriptToString(this.dir, options.auxScript)
  //     : "";
  //   const scriptInvalid = options.scriptInvalid ? "--script-invalid" : "";
  //   const witnessOverride = options.witnessOverride
  //     ? `--witness-override ${options.witnessOverride}`
  //     : "";
  //   if (!this.protocolParametersPath) this.queryProtocolParameters();
  //   this.runCommand(`${this.cliPath} transaction build \
  //               ${txInString} \
  //               ${txOutString} \
  //               ${txInCollateralString} \
  //               ${certs} \
  //               ${withdrawals} \
  //               ${mintString} \
  //               ${auxScript} \
  //               ${metadata} \
  //               ${scriptInvalid} \
  //               ${witnessOverride} \
  //               --invalid-hereafter ${
  //                 options.invalidAfter
  //                   ? options.invalidAfter
  //                   : this.queryTip().slot + 10000
  //               } \
  //               --invalid-before ${
  //                 options.invalidBefore ? options.invalidBefore : 0
  //               } \
  //               --out-file ${this.dir}/tmp/tx_${UID}.raw \
  //               ${changeAddressString} \
  //               --${this.network} \
  //               --protocol-params-file ${this.protocolParametersPath} \
  //               ${this.era}`);

  //   return `${this.dir}/tmp/tx_${UID}.raw`;
  // }
}
