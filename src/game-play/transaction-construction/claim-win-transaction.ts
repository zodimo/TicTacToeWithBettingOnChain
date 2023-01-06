import assert from "assert";
import { Game, GameIsWonPayout, Payout } from "../../app/game.js";
import { ClaimWinCommand, GameStateFactory } from "../../app/game-data.js";
import { RequiredSigner } from "../../cardano-cli/command/transaction/build/required-signer.js";
import { TxOut, TxOutParameter } from "../../cardano-cli/command/transaction/build/tx-out.js";
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
import { writeCommandParamsAsRedeemerToFile } from "./write-script-data-to-file.js";

export const sendClaimWinCommandToScriptTransaction: (
  playerWallet: Wallet,
  scriptAddress: string,
  scriptFile: string,
  scriptUtxoIdWithGameState: UtxoId,
  playerWallets: Wallet[]
) => UtxoId = (playerWallet, scriptAddress, scriptFile, gameStateUtxoId, playerWallets) => {
  console.log("###########################");
  console.log("######## CLAIM WIN ########");
  console.log("###########################");

  assert.equal(playerWallets.length == 2, true, "Expected both player wallets");

  const utxoFromStack = getUtxoFromScriptAddress(gameStateUtxoId, scriptAddress);

  const scriptData = fromJson(JSON.stringify(utxoFromStack.inlineDatum));
  const gameStateFromScriptData = new GameStateFactory().fromScriptData(scriptData);
  const playerPubKeyHash = cardanoCli.pubKeyHashFromVerificationKeyFile(playerWallet.keys.payment.verificationKeyFile);

  const command = new ClaimWinCommand(gameStateFromScriptData);
  //us payout to create outputs to transaction.
  const gamePayout = Game.handleEndGameActionCommand(command);
  const commandRedeemerile = writeCommandParamsAsRedeemerToFile(command);

  // const outputValueInLovelace = utxoFromStack.value.lovelace;

  const payOutToWallet: (payout: Payout, playerWallets: Wallet[]) => Wallet = (payout, wallets) => {
    const maybeIdentifiedWallet = wallets.find((wallet) => {
      return cardanoCli.pubKeyHashFromVerificationKeyFile(wallet.keys.payment.verificationKeyFile) == payout.pubKeyHash.toHexString();
    });
    assert.equal(!!maybeIdentifiedWallet, true, "No Wallet pub key hash matches!");
    return maybeIdentifiedWallet as Wallet;
  };

  const gamePayoutAsTxOutParameters: (gamePayOut: GameIsWonPayout, winningWallet: Wallet) => TxOutParameter = (
    gamePayout,
    wallet
  ) => {
    // @TODO improve this very naive approach
    // determine who the pubKeyHash Belongs to...
    // which value to use ? value in payout or on utxo ?

    const winningWalletPubKeyHash = cardanoCli.pubKeyHashFromVerificationKeyFile(
      wallet.keys.payment.verificationKeyFile
    );
    assert.equal(winningWalletPubKeyHash == gamePayout.payout.pubKeyHash.toHexString(), true, "Expects pub key hash to match!");

    const payoutInLoveLace = cardanoCli.toLovelace(gamePayout.payout.amountInAda);

    return new TxOutParameter(new TxOut(wallet.paymentAddr, payoutInLoveLace));
  };

  const gamePayoutInnerPayout = (gamePayout as GameIsWonPayout).payout;
  const winningWallet = payOutToWallet(gamePayoutInnerPayout, playerWallets);

  const transactionBuildOptions = new TransactionBuildOptions()
    .withTxIns(utxoStackAsInput(utxoStackForWallet(playerWallet))) //for fees
    .withTxIn(utxoAsTransactionBuildInput(utxoFromStack, commandRedeemerile, scriptFile)) //winning game input
    .withRequiredSigner(RequiredSigner.file(playerWallet.keys.payment.signingKeyFile))
    .withTxInCollateral(utxoStackForWallet(playerWallet).utxos[0].id.toString()) //just take the first- blindly
    .withTxOut(gamePayoutAsTxOutParameters(gamePayout as GameIsWonPayout, winningWallet))
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
  cardanoCli.waitForUtxoAtPaymentAddress(winningWallet.paymentAddr, txUtxoId, 600);
  return txUtxoId;
};
