import { Era } from "../../era.js";
import { Command } from "../command.js";
import { config } from "../../../cardano-cli-config.js";
import { NodeMode } from "../node-mode.js";
import { Network } from "../network.js";
import { Assertion } from "./build/assertion.js";
import { TxInParameter } from "./build/tx-in.js";
import { RequiredSigner } from "./build/required-signer.js";
import { TxOutParameter } from "./build/tx-out.js";

export class Build extends Command {
  // --*-era
  private era?: Era;
  // --*-mode
  private nodeMode?: NodeMode;
  // (--mainnet | --testnet-magic NATURAL)
  private network?: Network;
  // [--script-valid | --script-invalid]
  private assertion?: Assertion;
  //[--witness-override WORD]
  //(--tx-in TX-IN ...
  private txIns: TxInParameter[];

  // [--read-only-tx-in-reference TX-IN]
  private readOnlyTxInReference?: string;
  //[--required-signer FILE | --required-signer-hash HASH]
  private requiredSigner?: RequiredSigner;
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
  private protocolParamsFile?: string;
  //[--update-proposal-file FILE]
  private updateProposalFile?: string;
  // (--out-file FILE | --calculate-plutus-script-cost FILE)

  constructor(private commandPrefix: string) {
    super();
    this.era = config.getEra();
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

  withTxIn(txIn: TxInParameter): Build {
    this.txIns.push(txIn);
    return this;
  }

  withReadOnlyTxInReference(readOnlyTxInReference: string): Build {
    this.readOnlyTxInReference = readOnlyTxInReference;
    return this;
  }

  withRequiredSigner(requiredSigner: RequiredSigner): Build {
    this.requiredSigner = requiredSigner;
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

  withTxOuts(txOut: TxOutParameter): Build {
    this.txOuts.push(txOut);
    return this;
  }
  withChangeAddress(changeAddress: string): Build {
    this.changeAddress = changeAddress;
    return this;
  }

  getCommand(): string {
    let ouput: string[] = [this.commandPrefix, "build"];
    if (this.era) {
      ouput.push(this.era.asParameter());
    }
    if (this.nodeMode) {
      ouput.push(this.nodeMode.asParameter());
    }
    if (this.network) {
      ouput.push(this.network.asParameter());
    }
    if (this.assertion) {
      ouput.push(this.assertion.asParameter());
    }

    //tx-in
    this.txIns.map((txInParameter) => ouput.push(txInParameter.asParameter()));

    if (this.readOnlyTxInReference) {
      ouput.push(`--read-only-tx-in-reference ${this.readOnlyTxInReference}`);
    }

    if (this.requiredSigner) {
      ouput.push(this.requiredSigner.asParameter());
    }

    if (this.txInCollateral) {
      ouput.push(`--tx-in-collateral ${this.txInCollateral}`);
    }

    if (this.txOutReturnCollateral) {
      ouput.push(`--tx-out-return-collateral ${this.txOutReturnCollateral}`);
    }
    if (this.txTotalCollateral) {
      ouput.push(`--tx-total-collateral ${this.txTotalCollateral}`);
    }

    //tx-out
    this.txOuts.map((txOutParameter) =>
      ouput.push(txOutParameter.asParameter())
    );

    if (this.changeAddress) {
      ouput.push(`--change-address ${this.changeAddress}`);
    }

    return ouput.join(" ");
  }
}
