# dist/game-play/onchain-real/game3-cancel-initiated-game.js

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
Nobody joins the game and the initiated game can be cancelled after the timeout (gameMaxIntervalInSeconds) set on when initiated.

Anybody can cancel the game, the transaction fees will be paid by the wallet initiating the cancel request. 
In this case the locked value will be returned to the player 1 wallet.

```typescript
// signature
// const sendCancelInitiatedGameCommandToScriptTransaction: (
//   playerWallet: Wallet,
//   scriptAddress: string,
//   scriptFile: string,
//   scriptUtxoIdWithGameState: UtxoId,
//   playerWallets: Wallet[]
// ) => UtxoId
const tx2UtxoId = sendCancelInitiatedGameCommandToScriptTransaction(
  player1Wallet,
  scriptAddress,
  scriptFile,
  tx1UtxoId,
  [player1Wallet, player2Wallet]
);

console.log("##########################");
console.log(`The last utxos of the cancelled game : ${tx2UtxoId}`);

```