import { execSync } from "child_process";
import fs from "fs";
import { AddressKeys, ScriptWallet, Wallet, WalletKeys } from "./wallet.js";
import { UtxoId } from "./utxo-id.js";
import {
  StackValue,
  Utxo,
  UtxoNativeAsset,
  UtxoStack,
  UtxoValue,
} from "./utxo.js";

import { Network } from "./command/network.js";
import { PaymentAddressBuildOptions } from "./address.js";
import { Era } from "./era.js";
import { TransactionBuildOptions } from "./transaction-build.js";
import { TransactionBuildRawOptions } from "./transaction/buid-raw-options.js";
import { TransactionCalculateMinFeeOptions } from "./transaction/calculate-min-fee-options.js";
import { TransactionSignOptions } from "./transaction/sign-options.js";
import { TransactionSubmitOptions } from "./transaction/submit-options.js";
import { TxIdOptions } from "./transaction/tx-id-options.js";
import { Query } from "./command/query.js";
import { runCommand } from "./run-command.js";
import { ensureTempDirectoryExists, createTempFilename } from "./temp-dir.js";
import { Transaction } from "./command/transaction.js";
import { NodeMode } from "./command/node-mode.js";
import { OutFile } from "./command/shared/out-file.js";
import { Filter } from "./command/query/utxo/filter.js";
import { ProtocolParamsFile } from "./command/shared/protocol-params-file.js";
import { SigningKeyFile } from "./command/transaction/sign/signing-key-file.js";
import { TxIdTx } from "./command/transaction/tx-id.js";

export interface CardanoCliOptionsInterface {
  cliPath: string | null;
  dir: string;
  era: Era;
  network: Network;
  debug: boolean | null;
  nodeMode: NodeMode | null;
}

export class CardanoCliOptions implements CardanoCliOptionsInterface {
  constructor(
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

    this.era = options.era;
    this.network = options.network;
    options.dir && (this.dir = options.dir);
    options.cliPath && (this.cliPath = options.cliPath);
    options.nodeMode && (this.nodeMode = options.nodeMode);

    this.protocolParametersFile = this.createTempFilename(
      "protocolParams.json"
    );

    ensureTempDirectoryExists();

    this.writeProtocolParametersFile();
    this.ensureProtocolParametersFileExist();
  }

  private createTempFilename(filename: string): string {
    return createTempFilename(filename);
  }

  private runCommand(command: string): string {
    return runCommand(command);
  }

  query(): Query {
    // low level command
    return new Query(this.cliPath);
  }

  transaction(): Transaction {
    // low level command
    return new Transaction(this.cliPath);
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

  getUtxoStackForAddress(paymentAddr: string): UtxoStack {
    const utxos = this.getUtxoListForAddress(paymentAddr);
    return new UtxoStack(utxos);
  }

  getUtxoListForAddress(address: string): Utxo[] {
    const UID = Math.random().toString(36).slice(2, 9);
    const utxosTempFile = this.createTempFilename(`utxo_${UID}.json`);

    this.query()
      .utxo((builder) => {
        builder.withOutFile(new OutFile(utxosTempFile));
        builder.withFilter(Filter.address(address));
        return builder;
      })
      .runCommand();

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
    //higher order function

    let UID = Math.random().toString(36).slice(2, 9);
    const outFileName = createTempFilename(`tx_${UID}.raw`);

    this.transaction()
      .buildRaw((builder) => {
        //tx-in
        options.getTxIns().map((txInParameter) => {
          builder.withTxIn(txInParameter);
        });
        //tx-out
        options.getTxOuts().map((txOutParameter) => {
          builder.withTxOut(txOutParameter);
        });
        builder.withFee(options.getFee());
        builder.withOutFile(new OutFile(outFileName));
        builder.withProtocolParamsFile(
          new ProtocolParamsFile(this.protocolParametersFile)
        );
        return builder;
      })
      .runCommand();

    return outFileName;
  }

  writeProtocolParametersFile(): void {
    this.query()
      .protocolParameters((builder) => {
        builder.withOutFile(new OutFile(this.protocolParametersFile));
        return builder;
      })
      .runCommand();
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
    //higher order function
    return parseInt(
      this.transaction()
        .calculateMinFee((builder) => {
          builder
            .withTxBodyFile(options.txBodyFile)
            .withTxInCount(options.txInCount)
            .withTxOutCount(options.txOutCount)
            .withWitnessCount(options.witnessCount)
            .withProtocolParamsFile(
              new ProtocolParamsFile(this.protocolParametersFile)
            );

          if (options.byronWitnessCount) {
            builder.withByronWitnessCount(options.byronWitnessCount);
          }
          return builder;
        })
        .runCommand()
    );
  }

  transactionSign(options: TransactionSignOptions): string {
    //higher order function
    const UID = Math.random().toString(36).slice(2, 9);
    const outFile = this.createTempFilename(`tx_${UID}.signed`);
    this.transaction()
      .sign((builder) => {
        builder.withTxToSign(options.txToSign);
        options.signingKeyFiles.map((signingKeyFile) => {
          builder.withSigningKeyFile(signingKeyFile);
        });
        builder.withOutFile(new OutFile(outFile));
        return builder;
      })
      .runCommand();
    return `${outFile}`;
  }

  transactionSubmit(options: TransactionSubmitOptions) {
    //higher order function
    this.transaction()
      .submit((builder) => {
        builder.withTxFile(options.txFile);
        return builder;
      })
      .runCommand();

    return this.transactionTxid(new TxIdOptions(TxIdTx.file(options.txFile)));
  }

  transactionTxid(options: TxIdOptions): string {
    //higher order function
    return this.transaction()
      .txId((builder) => {
        builder.withTx(options.tx);
        return builder;
      })
      .runCommand();
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
      if (!this.getUtxoStackForAddress(paymentAddress).hasUtxo(utxoId)) {
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
