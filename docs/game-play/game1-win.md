# dist/game-play/onchain-real/game1-win.js

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

PLayer 2 is "psuedoly nominated to make the first move".

Player 2  plays A 1
```typescript
//    1   2   3
// A _O_|___|___
// B ___|___|___
// C    |   |

// signature
// const sendMakeMoveCommandToScriptTransaction: (
//   playerWallet: Wallet,
//   move: Move,
//   scriptAddress: string,
//   scriptFile: string,
//   scriptUtxoIdWithGameState: UtxoId
// ) => UtxoId
const tx3Move = new Move(Row.ROW_A, Column.Col_1);
const tx3UtxoId = sendMakeMoveCommandToScriptTransaction(player2Wallet, tx3Move, scriptAddress, scriptFile, tx2UtxoId);
```

## TX4

Player 1  plays B 2

```typescript
// A _O_|___|___
// B ___|_X_|___
// C    |   |

const tx4Move = new Move(Row.ROW_B, Column.Col_2);
const tx4UtxoId = sendMakeMoveCommandToScriptTransaction(player1Wallet, tx4Move, scriptAddress, scriptFile, tx3UtxoId);
```

## TX5

Player 2  plays A 2

```typescript
// A _O_|_O_|___
// B ___|_X_|___
// C    |   |

const tx5Move = new Move(Row.ROW_A, Column.Col_2);
const tx5UtxoId = sendMakeMoveCommandToScriptTransaction(player2Wallet, tx5Move, scriptAddress, scriptFile, tx4UtxoId);
```

## TX6

Player 1  plays B 3

```typescript
// A _O_|_O_|___
// B ___|_X_|_X_
// C    |   |

const tx6Move = new Move(Row.ROW_B, Column.Col_3);
const tx6UtxoId = sendMakeMoveCommandToScriptTransaction(player1Wallet, tx6Move, scriptAddress, scriptFile, tx5UtxoId);
```

## TX7

Player 2  plays A 3 for the WIN

```typescript
// A _O_|_O_|_O_
// B ___|_X_|_X_
// C    |   |

const tx7Move = new Move(Row.ROW_A, Column.Col_3);
const tx7UtxoId = sendMakeMoveCommandToScriptTransaction(player2Wallet, tx7Move, scriptAddress, scriptFile, tx6UtxoId);
```

## TX8

Any player can claim the win and will then also pay the fees.  
The transaction will pay the locked value to the winning address.

```typescript
// signature
// const sendClaimWinCommandToScriptTransaction: (
//   playerWallet: Wallet,
//   scriptAddress: string,
//   scriptFile: string,
//   scriptUtxoIdWithGameState: UtxoId,
//   playerWallets: Wallet[]
// ) => UtxoId
const tx8UtxoId = sendClaimWinCommandToScriptTransaction(player2Wallet, scriptAddress, scriptFile, tx7UtxoId, [
  player1Wallet,
  player2Wallet,
]);
console.log("##########################");
console.log(`The last utxo of the winning game : ${tx8UtxoId}`);
```
