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
} from "../../app/game-data.js";
import { Game } from "../../app/game.js";
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
  command: StartGameCommand,
  scriptAddress: string
) => UtxoId = (playerWallet, command, scriptAddress) => {
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
  cardanoCli.waitForUtxoAtPaymentAddress(scriptAddress, txUtxoId);
  return txUtxoId;
};

// ////////////
// // TX 1
// ////////////
// const playerOnePubKeyHash = cardanoCli.pubKeyHashFromVerificationKeyFile(
//   player1Wallet.keys.payment.verificationKeyFile
// );

// const startGameParams = new StartGameParams(playerOnePubKeyHash, 50, 1); // 1 second interval
// const tx1Command = new StartGameCommand(startGameParams);
// const tx1GameState = Game.handleActionCommand(tx1Command);
// const tx1GameStateDatumFile = writeGameStateAsDatumToFile(tx1GameState);

// const sendBetInLovelaceToScript = cardanoCli.toLovelace(tx1Command.getParameters().betInAda);
// //calculate change without fees to start with
// let player1WalletChangeInLovelace =
//   cardanoCli.getUtxoStackForAddress(player1Wallet.paymentAddr).getLoveLaceValue() - sendBetInLovelaceToScript;

// // add all utxos on payment adress as input
// const paymentAddressAsInput: (paymentAddress: string) => TxInParameter[] = (paymentAddress) => {
//   return cardanoCli.getUtxoListForAddress(paymentAddress).map((utxo) => new TxInParameter(new TxIn(utxo.id)));
// };

// // build
// const createTransactionBuildOptionsForChangeAndFee: (
//   changeInLovelace: number,
//   fee: number
// ) => TransactionBuildOptions = (changeInLovelace, fee) => {
//   return new TransactionBuildOptions()
//     .withTxIns(paymentAddressAsInput(player1Wallet.paymentAddr))
//     .withTxOuts([
//       new TxOutParameter(
//         new TxOut(scriptAddress, sendBetInLovelaceToScript),
//         TxOutDatum.inlineFile(tx1GameStateDatumFile)
//       ),
//     ])
//     .withChangeAddress(player1Wallet.paymentAddr);
// };

// //draft transaction
// const tx1BodyToSign = cardanoCli.transactionBuild(
//   createTransactionBuildOptionsForChangeAndFee(player1WalletChangeInLovelace, 0)
// );

// //sign

// const tx1SignedTransactionFile = cardanoCli.transactionSign(
//   new TransactionSignOptions(TxToSign.txBodyFile(tx1BodyToSign), [
//     new SigningKeyFile(player1Wallet.keys.payment.signingKeyFile),
//   ])
// );
// //send
// const tx1Hash = cardanoCli.transactionSubmit(new TransactionSubmitOptions(tx1SignedTransactionFile));
// const tx1UtxoId = new UtxoId(tx1Hash, 0);

// //wait for transaction to arrive
// cardanoCli.waitForUtxoAtPaymentAddress(scriptAddress, tx1UtxoId);
// console.log(`Utxo [${tx1UtxoId.toString()}] found at paymentAddess ${scriptAddress}`);

// ///////////////////////////////////////////////
// //      ONLY UtxoId crosses the line
// //////////////////////////////////////////////

// ////////////
// // TX 2
// ////////////

// // should we use the known UtxoId for it or find it on script address?

// //function to get the utxo to use :)

// // const tx1UtxoId = UtxoId.fromString("d7c4701ff30d807e41eb3d7a7434ad76d597c391709a5663979959abcc185d7d#1");
// const tx1UtxoFromStack = getUtxoFromScriptAddress(tx1UtxoId, scriptAddress);

// const tx1ScriptData = fromJson(JSON.stringify(tx1UtxoFromStack.inlineDatum));
// const tx1GameStateFromScriptData = new GameStateFactory().fromScriptData(tx1ScriptData);
// const playerTwoPubKeyHash = cardanoCli.pubKeyHashFromVerificationKeyFile(
//   player2Wallet.keys.payment.verificationKeyFile
// );
// const joinGameParams = new JoinGameParams(playerTwoPubKeyHash);
// const tx2Command = new JoinGameCommand(tx1GameStateFromScriptData, joinGameParams);
// const tx2GameState = Game.handleActionCommand(tx2Command);
// const tx2GameStateDatumFile = writeGameStateAsDatumToFile(tx2GameState);
// const tx2CommandRedeemerile = writeCommandParamsAsRedeemerToFile(tx2Command);

// //consume utxo with inline datum
// //and send it back to the script with the new datum
// const tx2outputValueInLovelace = tx1UtxoFromStack.value.lovelace + cardanoCli.toLovelace(tx2Command.betInAda);

// const tx2TransactionBuildOptions = new TransactionBuildOptions();
// tx2TransactionBuildOptions.withTxIns(utxoStackAsInput(utxoStackForWallet(player2Wallet)));
// tx2TransactionBuildOptions.withTxIn(utxoAsTransactionBuildInput(tx1UtxoFromStack, tx2CommandRedeemerile));
// tx2TransactionBuildOptions.withRequiredSigner(RequiredSigner.file(player2Wallet.keys.payment.signingKeyFile));
// tx2TransactionBuildOptions.withTxInCollateral(utxoStackForWallet(player2Wallet).utxos[0].id.toString()); //just take the first- blindly
// tx2TransactionBuildOptions.withTxOut(
//   new TxOutParameter(new TxOut(scriptAddress, tx2outputValueInLovelace), TxOutDatum.inlineFile(tx2GameStateDatumFile))
// );
// tx2TransactionBuildOptions.withChangeAddress(player2Wallet.paymentAddr);

// //draft transaction
// const tx2DraftTransactionBodyFile = cardanoCli.transactionBuild(tx2TransactionBuildOptions);
// // sign transaction
// const tx2SignedTransactionFile = cardanoCli.transactionSign(
//   new TransactionSignOptions(TxToSign.txBodyFile(tx2DraftTransactionBodyFile), [
//     new SigningKeyFile(player2Wallet.keys.payment.signingKeyFile),
//   ])
// );

// //send
// const tx2Hash = cardanoCli.transactionSubmit(new TransactionSubmitOptions(tx2SignedTransactionFile));
// const tx2UtxoId = new UtxoId(tx2Hash, 0);

// //wait for transaction to arrive
// cardanoCli.waitForUtxoAtPaymentAddress(scriptAddress, tx2UtxoId);
// console.log(`Utxo [${tx2UtxoId.toString()}] found at paymentAddess ${scriptAddress}`);

///////////////////////////////////////////////
//      ONLY UtxoId crosses the line
//////////////////////////////////////////////

const tx2UtxoId = UtxoId.fromString("83438c74b2cf2f9b196111d6ab7560762e94131283761fc28d258ed91efd33cb#0");
console.log(tx2UtxoId);
const tx2UtxoFromStack = getUtxoFromScriptAddress(tx2UtxoId, scriptAddress);

console.log(JSON.stringify(tx2UtxoFromStack, null, 2));

////////////
// TX 3
////////////
// // fake game start with player 2 as O and move A 1

// //    1   2   3
// // A _O_|___|___
// // B ___|___|___
// // C    |   |

// const move1 = new Move(Row.ROW_A, Column.Col_1);
// const makeMoveParams1 = new MakeMoveParams(playerTwoAddress, move1);
// const tx3: (gameState: GameState, makeMoveParams: MakeMoveParams) => GameState = (gamestate, makeMoveParams) => {
//   //Make first move
//   return Game.loadGame(gamestate).makeFirstMove(makeMoveParams).gameState;
// };
// const tx2GameStateFromScriptData = new GameStateFactory().fromScriptData(tx2GameStateAsScriptData);
// const tx3GameState = tx3(tx2GameStateFromScriptData, makeMoveParams1);
// const tx3GameStateAsScriptData = tx3GameState.toScriptData();
// console.log("##########################");
// console.log(`tx3 [${tx3GameState.constructor.name}]: ${JSON.stringify(tx3GameState, null, 2)}`);
// console.log(
//   `tx3 scriptdata : ${tx3GameStateAsScriptData.toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema)}`
// );

// ///////////////////////////////////////////////
// //      ONLY ScriptData crosses the line
// //////////////////////////////////////////////

// // TX 4
// // Player 1 Move B 2
// //    1   2   3

// // A _O_|___|___
// // B ___|_X_|___
// // C    |   |

// const move2 = new Move(Row.ROW_B, Column.Col_2);
// const makeMoveParams2 = new MakeMoveParams(playerOneAddress, move2);
// const tx4: (gameState: GameState, makeMoveParams: MakeMoveParams) => GameState = (gamestate, makeMoveParams) => {
//   return Game.loadGame(gamestate).makeMove(makeMoveParams).gameState;
// };

// const tx3GameStateFromScriptData = new GameStateFactory().fromScriptData(tx3GameStateAsScriptData);
// const tx4GameState = tx4(tx3GameStateFromScriptData, makeMoveParams2);
// const tx4GameStateAsScriptData = tx4GameState.toScriptData();
// console.log("##########################");
// console.log(`tx4 [${tx4GameState.constructor.name}]: ${JSON.stringify(tx4GameState, null, 2)}`);
// console.log(
//   `tx4 scriptdata : ${tx4GameStateAsScriptData.toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema)}`
// );

// ///////////////////////////////////////////////
// //      ONLY ScriptData crosses the line
// //////////////////////////////////////////////

// // TX 5
// // Player 2 Move A 2

// // A _O_|_O_|___
// // B ___|_X_|___
// // C    |   |

// const move3 = new Move(Row.ROW_A, Column.Col_2);
// const makeMoveParams3 = new MakeMoveParams(playerTwoAddress, move3);
// const tx5: (gameState: GameState, makeMoveParams: MakeMoveParams) => GameState = (gamestate, makeMoveParams) => {
//   return Game.loadGame(gamestate).makeMove(makeMoveParams).gameState;
// };

// const tx4GameStateFromScriptData = new GameStateFactory().fromScriptData(tx4GameStateAsScriptData);
// const tx5GameState = tx5(tx4GameStateFromScriptData, makeMoveParams3);
// const tx5GameStateAsScriptData = tx5GameState.toScriptData();
// console.log("##########################");
// console.log(`tx5 [${tx5GameState.constructor.name}]: ${JSON.stringify(tx5GameState, null, 2)}`);
// console.log(
//   `tx5 scriptdata : ${tx5GameStateAsScriptData.toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema)}`
// );

// ///////////////////////////////////////////////
// //      ONLY ScriptData crosses the line
// //////////////////////////////////////////////

// // TX 6
// // Player 1 Move B 3

// // A _O_|_O_|___
// // B ___|_X_|_X_
// // C    |   |

// const move4 = new Move(Row.ROW_B, Column.Col_3);
// const makeMoveParams4 = new MakeMoveParams(playerOneAddress, move4);
// const tx6: (gameState: GameState, makeMoveParams: MakeMoveParams) => GameState = (gamestate, makeMoveParams) => {
//   return Game.loadGame(gamestate).makeMove(makeMoveParams).gameState;
// };

// const tx5GameStateFromScriptData = new GameStateFactory().fromScriptData(tx5GameStateAsScriptData);
// const tx6GameState = tx6(tx5GameStateFromScriptData, makeMoveParams4);
// const tx6GameStateAsScriptData = tx6GameState.toScriptData();
// console.log("##########################");
// console.log(`tx6 [${tx6GameState.constructor.name}]: ${JSON.stringify(tx6GameState, null, 2)}`);
// console.log(
//   `tx6 scriptdata : ${tx6GameStateAsScriptData.toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema)}`
// );

// ///////////////////////////////////////////////
// //      ONLY ScriptData crosses the line
// //////////////////////////////////////////////

// // TX 7
// // Player 2 Move A 3

// // A _O_|_O_|_O_
// // B ___|_X_|_X_
// // C    |   |

// const move5 = new Move(Row.ROW_A, Column.Col_3);
// const makeMoveParams5 = new MakeMoveParams(playerTwoAddress, move5);
// const tx7: (gameState: GameState, makeMoveParams: MakeMoveParams) => GameState = (gamestate, makeMoveParams) => {
//   return Game.loadGame(gamestate).makeMove(makeMoveParams).gameState;
// };

// const tx6GameStateFromScriptData = new GameStateFactory().fromScriptData(tx6GameStateAsScriptData);
// const tx7GameState = tx7(tx6GameStateFromScriptData, makeMoveParams5);
// const tx7GameStateAsScriptData = tx7GameState.toScriptData();
// console.log("##########################");
// console.log(`tx7 [${tx7GameState.constructor.name}]: ${JSON.stringify(tx7GameState, null, 2)}`);
// console.log(
//   `tx7 scriptdata : ${tx7GameStateAsScriptData.toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema)}`
// );

// ///////////////////////////////////////////////
// //      ONLY ScriptData crosses the line
// //////////////////////////////////////////////

// const tx7GameStateFromScriptData = new GameStateFactory().fromScriptData(tx7GameStateAsScriptData);
// const tx8: (gameState: GameState) => void = (gamestate) => {
//   Game.loadGame(gamestate).claimWin();
// };
// // TX 8 Claim Win
// console.log("##########################");
// tx8(tx7GameStateFromScriptData);
