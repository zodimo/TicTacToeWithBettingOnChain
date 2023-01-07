# dist/game-play/onchain-real/game4-cancel-inprogress-game.js

## TX1

The game is started by player1 betting 5Ada with 1second timout.

```typescript
// signature
// const sendStartGameCommandToScriptTransaction: (
//   playerWallet: Wallet,
//   betInAda: number,
//   gameMaxIntervalInSeconds: number,
//   scriptAddress: string
// ) => UtxoId
const tx1UtxoId = sendStartGameCommandToScriptTransaction(player1Wallet, 5, 1, scriptAddress);
```

## TX2

Player 2 join the game and the transaction matches the bet found in the inline datum on tx1UtxoId.

```typescript
// signature
// const sendJoinGameCommandToScriptTransaction: (
//   playerWallet: Wallet,
//   scriptAddress: string,
//   scriptFile: string,
//   scriptUtxoIdWithGameState: UtxoId
// ) => UtxoId
const tx2UtxoId = sendJoinGameCommandToScriptTransaction(player2Wallet, scriptAddress, scriptFile, tx1UtxoId);
```

## TX3
Whenever a player fails to response (timeout set on game initiation gameMaxIntervalInSeconds) the game can then be cancelled and the waiting player will be the winner. 

Anybody can cancel the game, the transaction fees will be paid by the wallet initiating the cancel request. 
The winnder will be determined and the value locked will be paid to the winning player wallet.


```typescript
// signature
// const sendCancelInProgressGameCommandToScriptTransaction: (
//   playerWallet: Wallet,
//   scriptAddress: string,
//   scriptFile: string,
//   scriptUtxoIdWithGameState: UtxoId,
//   playerWallets: Wallet[]
// ) => UtxoId
const tx3UtxoId = sendCancelInProgressGameCommandToScriptTransaction(
  player2Wallet,
  scriptAddress,
  scriptFile,
  tx2UtxoId,
  [player1Wallet, player2Wallet]
);
console.log("##########################");
console.log(`The last utxo of the cancelled game : ${tx3UtxoId}`);

```