import { Era } from "../era.js";
import { Command } from "../command.js";
import { config } from "../../../cardano-cli-config.js";
import { NodeMode } from "../node-mode.js";
import { Network } from "../network.js";
import { Assertion } from "./shared/assertion.js";
import { TxInParameter } from "./build/tx-in.js";
import { RequiredSigner } from "./build/required-signer.js";
import { TxOutParameter } from "./build/tx-out.js";
import { ProtocolParamsFile } from "../shared/protocol-params-file.js";
import { OutputAs } from "./build/output-as.js";

export class Build extends Command {
  // --*-era
  private era?: Era;
  // --*-mode
  private nodeMode?: NodeMode;
  // (--mainnet | --testnet-magic NATURAL)
  private network?: Network;
  // [--script-valid | --script-invalid]
  private assertion?: Assertion;
  // [--witness-override WORD]
  private witnessOverride?: string;
  //(--tx-in TX-IN ...
  private txIns: TxInParameter[];
  // [--read-only-tx-in-reference TX-IN]
  private readOnlyTxInReference?: string;
  //[--required-signer FILE | --required-signer-hash HASH]
  private requiredSigners?: RequiredSigner[];
  //[--tx-in-collateral TX-IN]
  private txInCollateral?: string;
  //[--tx-out-return-collateral ADDRESS VALUE]
  private txOutReturnCollateral?: string;
  //[--tx-total-collateral INTEGER]
  private txTotalCollateral?: number;
  // [--tx-out ADDRESS VALUE ...
  private txOuts: TxOutParameter[];
  //--change-address ADDRESS
  private changeAddress?: string;
  //[--mint VALUE ...
  private mints?: [];
  //[--invalid-before SLOT]
  private invalidBefore?: number;
  //[--invalid-hereafter SLOT]
  private invalidHereafter?: number;
  //[--certificate-file CERTIFICATEFILE ...
  private certificateFiles?: [];
  // [--withdrawal WITHDRAWAL ...
  private withdrawels?: [];
  //[--json-metadata-no-schema | --json-metadata-detailed-schema]
  private jsonMetadataSchema?: string;
  //[--auxiliary-script-file FILE]
  private AuxiliaryScriptFile?: string;
  //[--metadata-json-file FILE | --metadata-cbor-file FILE]
  private metadataFile?: string;
  //[--protocol-params-file FILE]
  private protocolParamsFile?: ProtocolParamsFile;
  //[--update-proposal-file FILE]
  private updateProposalFile?: string;
  // (--out-file FILE | --calculate-plutus-script-cost FILE)
  private outputAs?: OutputAs;

  constructor(private commandPrefix: string) {
    super();
    this.era = config.getEra();
    this.network = config.getNetwork();
    this.txIns = [];
    this.txOuts = [];
  }

  withEra(era: Era): Build {
    this.era = era;
    return this;
  }

  withNodeMode(nodeMode: NodeMode): Build {
    this.nodeMode = nodeMode;
    return this;
  }

  withNetwork(network: Network): Build {
    this.network = network;
    return this;
  }

  withAssertion(assertion: Assertion): Build {
    this.assertion = assertion;
    return this;
  }

  withTxIns(txIns: TxInParameter[]): Build {
    this.txIns = txIns;
    return this;
  }

  withTxIn(txIn: TxInParameter): Build {
    this.txIns.push(txIn);
    return this;
  }

  withReadOnlyTxInReference(readOnlyTxInReference: string): Build {
    this.readOnlyTxInReference = readOnlyTxInReference;
    return this;
  }

  withRequiredSigners(requiredSigners: RequiredSigner[]): Build {
    this.requiredSigners = requiredSigners;
    return this;
  }
  withRequiredSigner(requiredSigner: RequiredSigner): Build {
    if (!this.requiredSigners) {
      this.requiredSigners = [];
    }
    this.requiredSigners.push(requiredSigner);
    return this;
  }

  withTxInCollateral(txInCollateral: string): Build {
    this.txInCollateral = txInCollateral;
    return this;
  }

  withTxOutReturnCollateral(txOutReturnCollateral: string): Build {
    this.txOutReturnCollateral = txOutReturnCollateral;
    return this;
  }

  withTxTotalCollateral(txTotalCollateral: number): Build {
    this.txTotalCollateral = txTotalCollateral;
    return this;
  }

  withTxOuts(txOuts: TxOutParameter[]): Build {
    this.txOuts = txOuts;
    return this;
  }

  withTxOut(txOut: TxOutParameter): Build {
    this.txOuts.push(txOut);
    return this;
  }

  withChangeAddress(changeAddress: string): Build {
    this.changeAddress = changeAddress;
    return this;
  }

  withInvalidBefore(invalidBefore: number): Build {
    this.invalidBefore = invalidBefore;
    return this;
  }

  withInvalidHereafter(invalidHereafter: number): Build {
    this.invalidHereafter = invalidHereafter;
    return this;
  }

  withProtocolParamsFile(protocolParamsFile: ProtocolParamsFile): Build {
    this.protocolParamsFile = protocolParamsFile;
    return this;
  }

  withOutputAs(outputAs: OutputAs): Build {
    this.outputAs = outputAs;
    return this;
  }

  getCommand(): string {
    let output: string[] = [this.commandPrefix, "build"];
    if (this.era) {
      output.push(this.era.asParameter());
    }
    if (this.nodeMode) {
      output.push(this.nodeMode.asParameter());
    }
    if (this.network) {
      output.push(this.network.asParameter());
    }
    if (this.assertion) {
      output.push(this.assertion.asParameter());
    }

    //tx-in
    this.txIns.forEach((txInParameter) => output.push(txInParameter.asParameter()));

    if (this.readOnlyTxInReference) {
      output.push(`--read-only-tx-in-reference ${this.readOnlyTxInReference}`);
    }

    if (this.requiredSigners) {
      this.requiredSigners.forEach((requiredSigner) => output.push(requiredSigner.asParameter()));
    }

    if (this.txInCollateral) {
      output.push(`--tx-in-collateral ${this.txInCollateral}`);
    }

    if (this.txOutReturnCollateral) {
      output.push(`--tx-out-return-collateral ${this.txOutReturnCollateral}`);
    }
    if (this.isset(this.txTotalCollateral)) {
      output.push(`--tx-total-collateral ${this.txTotalCollateral}`);
    }

    //tx-out
    this.txOuts.forEach((txOutParameter) => output.push(txOutParameter.asParameter()));

    if (this.changeAddress) {
      output.push(`--change-address ${this.changeAddress}`);
    }

    if (this.isset(this.invalidBefore)) {
      output.push(`--invalid-before ${this.invalidBefore}`);
    }

    if (this.isset(this.invalidHereafter)) {
      output.push(`--invalid-hereaftere ${this.invalidHereafter}`);
    }

    if (this.protocolParamsFile) {
      output.push(this.protocolParamsFile.asParameter());
    }

    if (this.outputAs) {
      output.push(this.outputAs.asParameter());
    }

    return output.join(" ");
  }
}
