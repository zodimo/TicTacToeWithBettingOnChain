import {
  Column,
  GameState,
  GameStateFactory,
  Move,
  Row,
  JoinGameParams,
  StartGameParams,
  MakeMoveParams,
  JoinGameCommand,
  ToRedeemerScriptData,
  StartGameCommand,
  GameActionCommand,
  MakeMoveCommand,
  ClaimWinCommand,
} from "../../app/game-data.js";
import { Game, GameIsWonPayout, GamePayOut, Payout } from "../../app/game.js";
import { fromJson, ScriptDataJsonSchema } from "../../cardano-cli/script-data.js";
import { createTempFilename } from "../../cardano-cli/temp-dir.js";
import { cardanoCli } from "../../previewCardanoCliJs.js";
import fs from "fs";
import {
  getScriptAddress,
  getScriptFile,
  getUntypedAlwaysSucceedScriptAddress,
  getUntypedAlwaysSucceedScriptFile,
} from "../../smart-contract.js";
import { TransactionBuildRawOptions } from "../../cardano-cli/transaction/buid-raw-options.js";
import { TxIn, TxInAdditional, TxInParameter } from "../../cardano-cli/command/transaction/build/tx-in.js";
import { TxOut, TxOutDatum } from "../../cardano-cli/command/transaction/build/tx-out.js";
import { TxOutParameter } from "../../cardano-cli/command/transaction/build/tx-out.js";
import { Fee } from "../../cardano-cli/command/shared/fee.js";
import { TransactionCalculateMinFeeOptions } from "../../cardano-cli/transaction/calculate-min-fee-options.js";
import { TxToSign } from "../../cardano-cli/command/transaction/sign/tx-to-sign.js";
import { TransactionSignOptions } from "../../cardano-cli/transaction/sign-options.js";
import { SigningKeyFile } from "../../cardano-cli/command/transaction/sign/signing-key-file.js";
import { UtxoId } from "../../cardano-cli/utxo-id.js";
import { TransactionSubmitOptions } from "../../cardano-cli/transaction/submit-options.js";
import { PaymentVerificationKey } from "../../cardano-cli/command/address/key-hash.js";
import assert from "assert";
import { Utxo, UtxoStack } from "../../cardano-cli/utxo.js";
import {
  TxInAlternativeFactory,
  TxInDatum,
  TxInRedeemer,
  TxInScriptAdditional,
} from "../../cardano-cli/command/transaction/build/tx-in.js";
import { TransactionBuildOptions } from "../../cardano-cli/transaction-build-options.js";
import { RequiredSigner } from "../../cardano-cli/command/transaction/build/required-signer.js";
import { Wallet } from "../../cardano-cli/wallet.js";

const player1Wallet = cardanoCli.wallet("player1");
const player2Wallet = cardanoCli.wallet("player2");
const scriptAddress = getUntypedAlwaysSucceedScriptAddress();
const scriptFile = getUntypedAlwaysSucceedScriptFile();

const utxoStackForWallet: (wallet: Wallet) => UtxoStack = (wallet) =>
  cardanoCli.getUtxoStackForAddress(wallet.paymentAddr);

const utxoStackAsInput: (utxoStack: UtxoStack) => TxInParameter[] = (utxoStack) => {
  return utxoStack.utxos.map((utxo) => {
    return new TxInParameter(
      new TxIn(utxo.id) // tx-in
    );
  });
};

const writeGameStateAsDatumToFile: (gameState: GameState) => string = (gameState) => {
  const UID = Math.random().toString(36).slice(2, 9);
  const tempDatumFile = createTempFilename(`datum_${UID}.json`);
  fs.writeFileSync(
    tempDatumFile,
    gameState.toScriptData().toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema)
  );
  return tempDatumFile;
};
const writeCommandParamsAsRedeemerToFile: (command: ToRedeemerScriptData) => string = (command) => {
  const UID = Math.random().toString(36).slice(2, 9);
  const tempDatumFile = createTempFilename(`redeemer_${UID}.json`);
  fs.writeFileSync(
    tempDatumFile,
    command.toRedeemerScriptData().toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema)
  );
  return tempDatumFile;
};

const utxoAsTransactionBuildInput: (utxo: Utxo, redeemerFile: string) => TxInParameter = (utxo, redeemerFile) => {
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

// add all utxos on payment adress as input
const paymentAddressAsInput: (paymentAddress: string) => TxInParameter[] = (paymentAddress) => {
  return cardanoCli.getUtxoListForAddress(paymentAddress).map((utxo) => new TxInParameter(new TxIn(utxo.id)));
};

const getUtxoFromScriptAddress: (utxoId: UtxoId, scriptAddress: string) => Utxo = (utxoId, scriptAddress) => {
  const scriptAdressUtxoStack = cardanoCli.getUtxoStackForAddress(scriptAddress);
  const maybeUtxoFromStack = scriptAdressUtxoStack.utxos.find((utxo) => {
    return utxo.id.equals(utxoId);
  });
  assert.equal(!!maybeUtxoFromStack, true, "Expects to find the utxo in the stack!");
  assert.equal(!!maybeUtxoFromStack?.inlineDatum, true, "Expexts to find the inline datum!");
  const utxo: Utxo = maybeUtxoFromStack as Utxo;
  return utxo;
};

const sendStartGameCommandToScriptTransaction: (
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
    .withTxIns(paymentAddressAsInput(player1Wallet.paymentAddr))
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

const sendJoinGameCommandToScriptTransaction: (
  playerWallet: Wallet,
  scriptAddress: string,
  scriptUtxoIdWithGameState: UtxoId
) => UtxoId = (playerWallet, scriptAddress, gameStateUtxcoId) => {
  console.log("###########################");
  console.log("######## JOIN GAME ########");
  console.log("###########################");

  const utxoFromStack = getUtxoFromScriptAddress(gameStateUtxcoId, scriptAddress);

  const scriptData = fromJson(JSON.stringify(utxoFromStack.inlineDatum));
  const gameStateFromScriptData = new GameStateFactory().fromScriptData(scriptData);
  const playerPubKeyHash = cardanoCli.pubKeyHashFromVerificationKeyFile(playerWallet.keys.payment.verificationKeyFile);

  const joinGameParams = new JoinGameParams(playerPubKeyHash);
  const command = new JoinGameCommand(gameStateFromScriptData, joinGameParams);
  const gameState = Game.handleActionCommand(command);
  const gameStateDatumFile = writeGameStateAsDatumToFile(gameState);
  const commandRedeemerile = writeCommandParamsAsRedeemerToFile(command);

  const outputValueInLovelace = utxoFromStack.value.lovelace + cardanoCli.toLovelace(command.betInAda);

  const transactionBuildOptions = new TransactionBuildOptions()
    .withTxIns(utxoStackAsInput(utxoStackForWallet(playerWallet)))
    .withTxIn(utxoAsTransactionBuildInput(utxoFromStack, commandRedeemerile))
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

const sendMakeMoveCommandToScriptTransaction: (
  playerWallet: Wallet,
  move: Move,
  scriptAddress: string,
  scriptUtxoIdWithGameState: UtxoId
) => UtxoId = (playerWallet, move, scriptAddress, gameStateUtxoId) => {
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
    .withTxIn(utxoAsTransactionBuildInput(utxoFromStack, commandRedeemerile))
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

const sendClaimWinCommandToScriptTransaction: (
  playerWallet: Wallet,
  scriptAddress: string,
  scriptUtxoIdWithGameState: UtxoId
) => UtxoId = (playerWallet, scriptAddress, gameStateUtxoId) => {
  console.log("###########################");
  console.log("######## CLAIM WIN ########");
  console.log("###########################");

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
      return cardanoCli.pubKeyHashFromVerificationKeyFile(wallet.keys.payment.verificationKeyFile) == payout.pubKeyHash;
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
    assert.equal(winningWalletPubKeyHash == gamePayout.payout.pubKeyHash, true, "Expects pub key hash to match!");

    const payoutInLoveLace = cardanoCli.toLovelace(gamePayout.payout.amountInAda);

    return new TxOutParameter(new TxOut(wallet.paymentAddr, payoutInLoveLace));
  };

  const gamePayoutInnerPayout = (gamePayout as GameIsWonPayout).payout;
  const winningWallet = payOutToWallet(gamePayoutInnerPayout, [player1Wallet, player2Wallet]);

  const transactionBuildOptions = new TransactionBuildOptions()
    .withTxIns(utxoStackAsInput(utxoStackForWallet(playerWallet))) //for fees
    .withTxIn(utxoAsTransactionBuildInput(utxoFromStack, commandRedeemerile)) //winning game input
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

////////////
// TX 1
////////////

const tx1UtxoId = sendStartGameCommandToScriptTransaction(player1Wallet, 5, 1, scriptAddress);

///////////////////////////////////////////////
//      ONLY UtxoId crosses the line
//////////////////////////////////////////////

////////////
// TX 2
////////////

const tx2UtxoId = sendJoinGameCommandToScriptTransaction(player2Wallet, scriptAddress, tx1UtxoId);

///////////////////////////////////////////////
//      ONLY UtxoId crosses the line
//////////////////////////////////////////////

////////////
// TX 3
////////////
// fake game start with player 2 as O and move A 1

//    1   2   3
// A _O_|___|___
// B ___|___|___
// C    |   |

const tx3Move = new Move(Row.ROW_A, Column.Col_1);
const tx3UtxoId = sendMakeMoveCommandToScriptTransaction(player2Wallet, tx3Move, scriptAddress, tx2UtxoId);

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

// TX 4
// Player 1 Move B 2
//    1   2   3

// A _O_|___|___
// B ___|_X_|___
// C    |   |

const tx4Move = new Move(Row.ROW_B, Column.Col_2);
const tx4UtxoId = sendMakeMoveCommandToScriptTransaction(player1Wallet, tx4Move, scriptAddress, tx3UtxoId);
// const tx4UtxoFromStack = getUtxoFromScriptAddress(tx4UtxoId, scriptAddress);
// console.log(JSON.stringify(tx4UtxoFromStack, null, 2));

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

// TX 5
// Player 2 Move A 2

// A _O_|_O_|___
// B ___|_X_|___
// C    |   |

const tx5Move = new Move(Row.ROW_A, Column.Col_2);
const tx5UtxoId = sendMakeMoveCommandToScriptTransaction(player2Wallet, tx5Move, scriptAddress, tx4UtxoId);
// const tx5UtxoFromStack = getUtxoFromScriptAddress(tx5UtxoId, scriptAddress);
// console.log(JSON.stringify(tx5UtxoFromStack, null, 2));

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

// TX 6
// Player 1 Move B 3

// A _O_|_O_|___
// B ___|_X_|_X_
// C    |   |

const tx6Move = new Move(Row.ROW_B, Column.Col_3);
const tx6UtxoId = sendMakeMoveCommandToScriptTransaction(player1Wallet, tx6Move, scriptAddress, tx5UtxoId);
// const tx6UtxoFromStack = getUtxoFromScriptAddress(tx6UtxoId, scriptAddress);
// console.log(JSON.stringify(tx6UtxoFromStack, null, 2));

// ///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

// TX 7
// Player 2 Move A 3

// A _O_|_O_|_O_
// B ___|_X_|_X_
// C    |   |

const tx7Move = new Move(Row.ROW_A, Column.Col_3);
const tx7UtxoId = sendMakeMoveCommandToScriptTransaction(player2Wallet, tx7Move, scriptAddress, tx6UtxoId);
// const tx7UtxoFromStack = getUtxoFromScriptAddress(tx7UtxoId, scriptAddress);
// console.log(JSON.stringify(tx7UtxoFromStack, null, 2));

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

//any player can call claim win :) to pay the fees.
// TX 8 Claim Win
const tx8UtxoId = sendClaimWinCommandToScriptTransaction(player2Wallet, scriptAddress, tx7UtxoId);
console.log("##########################");
console.log(`The last utxo of the winning game : ${tx8UtxoId}`);
