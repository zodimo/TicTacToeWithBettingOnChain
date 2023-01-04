import { Game } from "../../app/game.js";
import { StartGameCommand, StartGameParams } from "../../app/game-data.js";
import { TxOut, TxOutDatum, TxOutParameter } from "../../cardano-cli/command/transaction/build/tx-out.js";
import { SigningKeyFile } from "../../cardano-cli/command/transaction/sign/signing-key-file.js";
import { TxToSign } from "../../cardano-cli/command/transaction/sign/tx-to-sign.js";
import { TransactionBuildOptions } from "../../cardano-cli/transaction-build-options.js";
import { TransactionSignOptions } from "../../cardano-cli/transaction/sign-options.js";
import { TransactionSubmitOptions } from "../../cardano-cli/transaction/submit-options.js";
import { UtxoId } from "../../cardano-cli/utxo-id.js";
import { Wallet } from "../../cardano-cli/wallet.js";
import { cardanoCli } from "../../previewCardanoCliJs.js";
import { paymentAddressAsInput } from "./utils.js";
import { writeGameStateAsDatumToFile } from "./write-script-data-to-file.js";

export const sendStartGameCommandToScriptTransaction: (
  playerWallet: Wallet,
  betInAda: number,
  gameMaxIntervalInSeconds: number,
  scriptAddress: string
) => UtxoId = (playerWallet, betInAda, gameMaxIntervalInSeconds, scriptAddress) => {
  console.log("###########################");
  console.log("####### START GAME ########");
  console.log("###########################");

  const playerPubKeyHash = cardanoCli.pubKeyHashFromVerificationKeyFile(playerWallet.keys.payment.verificationKeyFile);

  const startGameParams = new StartGameParams(playerPubKeyHash, betInAda, gameMaxIntervalInSeconds);
  const command = new StartGameCommand(startGameParams);

  const gameState = Game.handleActionCommand(command);
  const gameStateDatumFile = writeGameStateAsDatumToFile(gameState);
  const sendBetInLovelaceToScript = cardanoCli.toLovelace(command.getParameters().betInAda);
  // build
  const transactionBuildOptions = new TransactionBuildOptions()
    .withTxIns(paymentAddressAsInput(playerWallet.paymentAddr))
    .withTxOuts([
      new TxOutParameter(
        new TxOut(scriptAddress, sendBetInLovelaceToScript),
        TxOutDatum.inlineFile(gameStateDatumFile)
      ),
    ])
    .withChangeAddress(playerWallet.paymentAddr);

  const txBodyToSign = cardanoCli.transactionBuild(transactionBuildOptions);
  //sign

  const txSignedTransactionFile = cardanoCli.transactionSign(
    new TransactionSignOptions(TxToSign.txBodyFile(txBodyToSign), [
      new SigningKeyFile(playerWallet.keys.payment.signingKeyFile),
    ])
  );
  //send
  const txHash = cardanoCli.transactionSubmit(new TransactionSubmitOptions(txSignedTransactionFile));
  const txUtxoId = new UtxoId(txHash, 0);

  //wait for transaction to arrive
  cardanoCli.waitForUtxoAtPaymentAddress(scriptAddress, txUtxoId, 600);
  return txUtxoId;
};
