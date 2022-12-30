import { Era } from "./command/era.js";
import { Network } from "./command/network.js";
import { NodeMode } from "./command/node-mode.js";
import { ProtocolParamsFile } from "./command/shared/protocol-params-file.js";
import { RequiredSigner } from "./command/transaction/build/required-signer.js";
import { TxInParameter } from "./command/transaction/build/tx-in";
import { TxOutParameter } from "./command/transaction/build/tx-out.js";
import { Assertion } from "./command/transaction/shared/assertion.js";

export class TransactionBuildOptions {
  private era?: Era;
  private nodeMode?: NodeMode;
  private network?: Network;
  private assertion?: Assertion;
  private txIns?: TxInParameter[];
  private readOnlyTxInReference?: string;
  private requiredSigners?: RequiredSigner[];
  private txInCollateral?: string;
  private txOutReturnCollateral?: string;
  private txTotalCollateral?: number;
  private txOuts?: TxOutParameter[];
  private changeAddress?: string;
  private mints?: [];
  private invalidBefore?: number;
  private invalidHereafter?: number;
  private certificateFiles?: [];
  private withdrawels?: [];
  private jsonMetadataSchema?: string;
  private AuxiliaryScriptFile?: string;
  private metadataFile?: string;
  private protocolParamsFile?: ProtocolParamsFile;
  private updateProposalFile?: string;

  withEra(era: Era): TransactionBuildOptions {
    this.era = era;
    return this;
  }
  hasEra(): boolean {
    return !!this.era;
  }
  getEra() {
    if (!this.era) throw new Error("Era is not defined!");
    return this.era;
  }

  withNodeMode(nodeMode: NodeMode): TransactionBuildOptions {
    this.nodeMode = nodeMode;
    return this;
  }
  hasNodeMode(): boolean {
    return !!this.nodeMode;
  }
  getNodeMode(): NodeMode {
    if (!this.nodeMode) throw new Error("NodeMode is not defined!");
    return this.nodeMode;
  }

  withNetwork(network: Network): TransactionBuildOptions {
    this.network = network;
    return this;
  }
  hasNetwork(): boolean {
    return !!this.network;
  }
  getNetwork(): Network {
    if (!this.network) throw new Error("Network is not defined!");
    return this.network;
  }

  withAssertion(assertion: Assertion): TransactionBuildOptions {
    this.assertion = assertion;
    return this;
  }
  hasAssertion(): boolean {
    return !!this.assertion;
  }
  getAssertion(): Assertion {
    if (!this.assertion) throw new Error("Assertion is not defined!");
    return this.assertion;
  }

  withTxIns(txIns: TxInParameter[]): TransactionBuildOptions {
    this.txIns = txIns;
    return this;
  }
  withTxIn(txIn: TxInParameter): TransactionBuildOptions {
    if (!this.txIns) {
      this.txIns = [];
    }
    this.txIns.push(txIn);
    return this;
  }
  hasTxIns(): boolean {
    return !!this.txIns;
  }
  getTxIns(): TxInParameter[] {
    if (!this.txIns) throw new Error("TxIns is not defined!");
    return this.txIns;
  }

  withReadOnlyTxInReference(
    readOnlyTxInReference: string
  ): TransactionBuildOptions {
    this.readOnlyTxInReference = readOnlyTxInReference;
    return this;
  }
  hasReadOnlyTxInReference(): boolean {
    return !!this.readOnlyTxInReference;
  }
  getReadOnlyTxInReference(): string {
    if (!this.readOnlyTxInReference)
      throw new Error("ReadOnlyTxInReference is not defined!");
    return this.readOnlyTxInReference;
  }

  withRequiredSigners(
    requiredSigners: RequiredSigner[]
  ): TransactionBuildOptions {
    this.requiredSigners = requiredSigners;
    return this;
  }
  withRequiredSigner(requiredSigner: RequiredSigner): TransactionBuildOptions {
    if (!this.requiredSigners) {
      this.requiredSigners = [];
    }
    this.requiredSigners.push(requiredSigner);
    return this;
  }
  hasRequiredSigners(): boolean {
    return !!this.requiredSigners;
  }
  getRequiredSigners(): RequiredSigner[] {
    if (!this.requiredSigners)
      throw new Error("RequiredSigners is not defined!");
    return this.requiredSigners;
  }

  withTxInCollateral(txInCollateral: string): TransactionBuildOptions {
    this.txInCollateral = txInCollateral;
    return this;
  }
  hasTxInCollateral(): boolean {
    return !!this.txInCollateral;
  }
  getTxInCollateral(): string {
    if (!this.txInCollateral) throw new Error("TxInCollateral is not defined!");
    return this.txInCollateral;
  }

  withTxOutReturnCollateral(
    txOutReturnCollateral: string
  ): TransactionBuildOptions {
    this.txOutReturnCollateral = txOutReturnCollateral;
    return this;
  }
  hasTxOutReturnCollateral(): boolean {
    return !!this.txOutReturnCollateral;
  }
  getTxOutReturnCollateral(): string {
    if (!this.txOutReturnCollateral)
      throw new Error("TxOutReturnCollateralollateral is not defined!");
    return this.txOutReturnCollateral;
  }

  withTxTotalCollateral(txTotalCollateral: number): TransactionBuildOptions {
    this.txTotalCollateral = txTotalCollateral;
    return this;
  }
  hasTxTotalCollateral(): boolean {
    return !!this.txTotalCollateral;
  }
  getTxTotalCollateral(): number {
    if (!this.txTotalCollateral)
      throw new Error("TxTotalCollateral is not defined!");
    return this.txTotalCollateral;
  }

  withTxOuts(txOuts: TxOutParameter[]): TransactionBuildOptions {
    this.txOuts = txOuts;
    return this;
  }
  withTxOut(txOut: TxOutParameter): TransactionBuildOptions {
    if (!this.txOuts) {
      this.txOuts = [];
    }
    this.txOuts.push(txOut);
    return this;
  }
  hasTxOuts(): boolean {
    return !!this.txOuts;
  }
  getTxOuts(): TxOutParameter[] {
    if (!this.txOuts) throw new Error("TxOuts is not defined!");
    return this.txOuts;
  }

  withChangeAddress(changeAddress: string): TransactionBuildOptions {
    this.changeAddress = changeAddress;
    return this;
  }
  hasChangeAddress(): boolean {
    return !!this.changeAddress;
  }
  getChangeAddress(): string {
    if (!this.changeAddress) throw new Error("ChangeAddress is not defined!");
    return this.changeAddress;
  }

  withInvalidBefore(invalidBefore: number): TransactionBuildOptions {
    this.invalidBefore = invalidBefore;
    return this;
  }
  hasInvalidBefore(): boolean {
    return !!this.invalidBefore;
  }
  getInvalidBefore(): number {
    if (!this.invalidBefore) throw new Error("InvalidBefore is not defined!");
    return this.invalidBefore;
  }

  withInvalidHereafter(invalidHereafter: number): TransactionBuildOptions {
    this.invalidHereafter = invalidHereafter;
    return this;
  }
  hasInvalidHereafter(): boolean {
    return !!this.invalidHereafter;
  }
  getInvalidHereafter(): number {
    if (!this.invalidHereafter)
      throw new Error("InvalidHereafter is not defined!");
    return this.invalidHereafter;
  }

  withProtocolParamsFile(
    protocolParamsFile: ProtocolParamsFile
  ): TransactionBuildOptions {
    this.protocolParamsFile = protocolParamsFile;
    return this;
  }
  hasProtocolParamsFile(): boolean {
    return !!this.protocolParamsFile;
  }
  getProtocolParamsFile(): ProtocolParamsFile {
    if (!this.protocolParamsFile)
      throw new Error("ProtocolParamsFile is not defined!");
    return this.protocolParamsFile;
  }
}
