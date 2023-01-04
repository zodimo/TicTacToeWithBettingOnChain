import { Game } from "../../app/game.js";
import { GameStateFactory, MakeMoveCommand, MakeMoveParams, Move } from "../../app/game-data.js";
import { RequiredSigner } from "../../cardano-cli/command/transaction/build/required-signer.js";
import { TxOut, TxOutDatum, TxOutParameter } from "../../cardano-cli/command/transaction/build/tx-out.js";
import { SigningKeyFile } from "../../cardano-cli/command/transaction/sign/signing-key-file.js";
import { TxToSign } from "../../cardano-cli/command/transaction/sign/tx-to-sign.js";
import { fromJson } from "../../cardano-cli/script-data.js";
import { TransactionBuildOptions } from "../../cardano-cli/transaction-build-options.js";
import { TransactionSignOptions } from "../../cardano-cli/transaction/sign-options.js";
import { TransactionSubmitOptions } from "../../cardano-cli/transaction/submit-options.js";
import { UtxoId } from "../../cardano-cli/utxo-id.js";
import { Wallet } from "../../cardano-cli/wallet.js";
import { cardanoCli } from "../../previewCardanoCliJs.js";
import {
  getUtxoFromScriptAddress,
  utxoAsTransactionBuildInput,
  utxoStackAsInput,
  utxoStackForWallet,
} from "./utils.js";
import { writeCommandParamsAsRedeemerToFile, writeGameStateAsDatumToFile } from "./write-script-data-to-file.js";

export const sendMakeMoveCommandToScriptTransaction: (
  playerWallet: Wallet,
  move: Move,
  scriptAddress: string,
  scriptFile: string,
  scriptUtxoIdWithGameState: UtxoId
) => UtxoId = (playerWallet, move, scriptAddress, scriptFile, gameStateUtxoId) => {
  console.log("###########################");
  console.log("######## MAKE MOVE ########");
  console.log("###########################");

  const utxoFromStack = getUtxoFromScriptAddress(gameStateUtxoId, scriptAddress);

  const scriptData = fromJson(JSON.stringify(utxoFromStack.inlineDatum));
  const gameStateFromScriptData = new GameStateFactory().fromScriptData(scriptData);
  const playerPubKeyHash = cardanoCli.pubKeyHashFromVerificationKeyFile(playerWallet.keys.payment.verificationKeyFile);

  const makeMoveParams = new MakeMoveParams(playerPubKeyHash, move);
  const command = new MakeMoveCommand(gameStateFromScriptData, makeMoveParams);
  const gameState = Game.handleActionCommand(command);
  const gameStateDatumFile = writeGameStateAsDatumToFile(gameState);
  const commandRedeemerile = writeCommandParamsAsRedeemerToFile(command);

  const outputValueInLovelace = utxoFromStack.value.lovelace;

  const transactionBuildOptions = new TransactionBuildOptions()
    .withTxIns(utxoStackAsInput(utxoStackForWallet(playerWallet)))
    .withTxIn(utxoAsTransactionBuildInput(utxoFromStack, commandRedeemerile, scriptFile))
    .withRequiredSigner(RequiredSigner.file(playerWallet.keys.payment.signingKeyFile))
    .withTxInCollateral(utxoStackForWallet(playerWallet).utxos[0].id.toString()) //just take the first- blindly
    .withTxOut(
      new TxOutParameter(new TxOut(scriptAddress, outputValueInLovelace), TxOutDatum.inlineFile(gameStateDatumFile))
    )
    .withChangeAddress(playerWallet.paymentAddr);

  //draft transaction
  const draftTransactionBodyFile = cardanoCli.transactionBuild(transactionBuildOptions);
  // sign transaction
  const signedTransactionFile = cardanoCli.transactionSign(
    new TransactionSignOptions(TxToSign.txBodyFile(draftTransactionBodyFile), [
      new SigningKeyFile(playerWallet.keys.payment.signingKeyFile),
    ])
  );
  //send
  const txHash = cardanoCli.transactionSubmit(new TransactionSubmitOptions(signedTransactionFile));
  const txUtxoId = new UtxoId(txHash, 0);

  //wait for transaction to arrive
  cardanoCli.waitForUtxoAtPaymentAddress(scriptAddress, txUtxoId, 600);
  return txUtxoId;
};
