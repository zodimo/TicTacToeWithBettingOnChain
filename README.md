# TicTacToeWithBettingOnChain with a twist

TicTacToe with betting on Cardano blockchain.
With a Twist. The secret souce is to get the top row A1,A2,A3 to win.
No other patterns will win.

## Grid

![grid](./docs/images//grid.jpg)

## Example Flow of a game.

(The images below are for illustrative purposes only and may differ from the actual implementation.)

Below are 8 transactions to complete a game.  
Two PLayers, Bob and Peter, wants to play a game of Tic Tac Toe with betting.  
Only Bob knows the secret of the game.  
Bob starts the game by betting 50Ada and setting the timeout of the game to 30min per round.  
![tx1](./docs/images/tx1.jpg)
Peter has 30 mins to join the game and match Bob's 50 Adas, else the game will be cancelled and Bob can reclaim his bet.
The App will randomly selects a player to start. e.g X
![tx2](./docs/images/tx2.jpg)
Bob has 30mins to make a move or the game will end in a tie.  
Bob moves to Row_A, Col_1
![tx3](./docs/images/tx3.jpg)
Peter has 30mins to make a move or Bob will win.  
Peter moves to Row_B, Col_2
![tx4](./docs/images/tx4.jpg)
Bob has 30mins to make a move or Peter will win.  
Bob moves to Row_A, Col_2
![tx5](./docs/images/tx5.jpg)
Peter has 30mins to make a move or Bob will win.  
Peter moves to Row_B, Col_3
![tx6](./docs/images/tx6.jpg)
Bob has 30mins to make a move or Peter will win.  
Bob moves to winning move of Row_A, Col_3
![tx7](./docs/images/tx7.jpg)
Bob can now claim the winnings.
![tx8](./docs/images/tx8.jpg)

# Instruction

The transaction construction will happen with cardanocli-js as individual steps to create the transactions to play the game and simulate async interactions.

### How to build the plutus script.

This project assumes that you have prior knowledge of cardano-cli and have a node running locally. It can only be used by cloning the repo.

- putus-apps tag for nix shell: v1.0.0
- inside the plutus apps nix-shell, navigate back to root of this project.
- run `cabal repl`
- run `:l Deploy`
- run `writeTicTacToeValidator` , this will write to testnet/ticTacToe.plutus

### Cardano-cli

This project is built on cardano-cli 1.35.4

### how to run the staged transactions

first we need to do some setup work.

- setup .env file with `scp .env.example .env` to use the defaults. Feel free to edit .env file to suite your setup.

```text
# default CARDANO_CLI_BIN_PATH="cardano-cli"
CARDANO_CLI_BIN_PATH="cardano-cli"
# default WORKING_DIRECTORY = "./workdir"
WORKING_DIRECTORY="/workdir"
# magic number for preview network is 2, preprod 1, not used if NETWORK_IS_TESTNET = false
#default NETWORK_TESTNET_MAGIC_NUMBER=2
NETWORK_TESTNET_MAGIC_NUMBER=2
#true, false
#default CARDANO_CLI_DEBUG=true
CARDANO_CLI_DEBUG=true
```

- install `node v18`
- run `npm install` in root of project
- run `npx tsc` to compile all the typscript files
- create player 1 and player 2 wallets by running `node ./dist/scripts/create-player-wallets.js`
- you can at any time print the player 1 and player 2 wallets by running `node ./dist/scripts/print-player-wallets.js`
- vitis the cardano [faucet](https://docs.cardano.org/cardano-testnet/tools/faucet/) and to fund the plauer wallets
- you are now ready to run the example scripts.
- run `node dist/game-play/onchain-real/game1-win.js` for and staged game where there is a winner
- run `node dist/game-play/onchain-real/game2-tie.js` for and staged game where it ends in a tie
- run `node dist/game-play/onchain-real/game3-cancel-initiated-game.js` to initiate and cancel a game.
- run `node dist/game-play/onchain-real/game4-cancel-inprogress-game.js` to initiate a game, join and then stop playing and cancel the game.

## Validator requirement implementation onchain

(if not implemented && True is used )

- [x] can join initiated game

  - [x] game initiated with valid parameters (enforced by types)
  - [x] initiated game value must match the bet value
  - [x] joining value must match the bet value
  - [x] output value goes to script
  - [x] output gamestate is valid
    - [x] playerTwoPubKeyHash value is added from command ( I know all the datum props need to be tested)

- [x] can make move

  - [x] current gamestate with valid params (enforced by types)
  - [x] the move in the command matches the next player to make move.
  - [x] ensure game is playable [ not won or tied , ensured by types]
  - [x] ensure designated space is not occupied
  - [x] check output game state valid after making move

    - [x] is GameWon, is GameTied or still progress

- [x] can claim win

  - [x] is game in winning state (enforced by types)
  - [x] ensure output value goes to winner address

- [x] can claim tie

  - [x] is game in tied state (enforced by types)
  - [x] ensure output values are split between players

- [ ] can cancel initiated game

  - [x] game state must be in initiated state ( ensure by types)
  - [ ] has enough time passed as specified.[stuck here with tx intervals]
  - [x] output value return to playerOne

- [ ] can cancel in progress game

  - [x] game state must be in in-progrogress state ( ensure by types)
  - [ ] has enough time passed as specified.[stuck here with tx intervals]
  - [x] determine winner
  - [x] output value goes to winner
