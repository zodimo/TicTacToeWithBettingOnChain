import assert from "assert";
import {
  TxIn,
  TxInAdditional,
  TxInAlternativeFactory,
  TxInDatum,
  TxInParameter,
  TxInRedeemer,
  TxInScriptAdditional,
} from "../../cardano-cli/command/transaction/build/tx-in.js";
import { Utxo, UtxoStack } from "../../cardano-cli/utxo.js";
import { UtxoId } from "../../cardano-cli/utxo-id.js";
import { Wallet } from "../../cardano-cli/wallet.js";
import { cardanoCli } from "../../previewCardanoCliJs.js";

// add all utxos on payment adress as input
export const paymentAddressAsInput: (paymentAddress: string) => TxInParameter[] = (paymentAddress) => {
  return cardanoCli.getUtxoListForAddress(paymentAddress).map((utxo) => new TxInParameter(new TxIn(utxo.id)));
};

export const getUtxoFromScriptAddress: (utxoId: UtxoId, scriptAddress: string) => Utxo = (utxoId, scriptAddress) => {
  const scriptAdressUtxoStack = cardanoCli.getUtxoStackForAddress(scriptAddress);
  const maybeUtxoFromStack = scriptAdressUtxoStack.utxos.find((utxo) => {
    return utxo.id.equals(utxoId);
  });
  assert.equal(!!maybeUtxoFromStack, true, "Expects to find the utxo in the stack!");
  assert.equal(!!maybeUtxoFromStack?.inlineDatum, true, "Expexts to find the inline datum!");
  const utxo: Utxo = maybeUtxoFromStack as Utxo;
  return utxo;
};

export const utxoStackAsInput: (utxoStack: UtxoStack) => TxInParameter[] = (utxoStack) => {
  return utxoStack.utxos.map((utxo) => {
    return new TxInParameter(
      new TxIn(utxo.id) // tx-in
    );
  });
};

export const utxoStackForWallet: (wallet: Wallet) => UtxoStack = (wallet) =>
  cardanoCli.getUtxoStackForAddress(wallet.paymentAddr);

export const utxoAsTransactionBuildInput: (utxo: Utxo, redeemerFile: string, scriptFile: string) => TxInParameter = (
  utxo,
  redeemerFile,
  scriptFile
) => {
  return new TxInParameter(
    new TxIn(utxo.id), // tx-in
    new TxInAdditional(
      TxInAlternativeFactory.txInScriptFile(scriptFile),
      new TxInScriptAdditional()
        .withTxInDatum(TxInDatum.inlineDatumInPresent())
        .withTxInRedeemer(TxInRedeemer.file(redeemerFile))
    )
  );
};
