import { Column, GameState, GameStateFactory, Move, Row } from "../../app/game-data.js";
import { Game, JoinGameParams, StartGameParams, MakeMoveParams } from "../../app/game.js";
import { ScriptDataJsonSchema } from "../../cardano-cli/script-data.js";

// TX 1
const playerOneAddress = "Player1Address";
const startGameParams = new StartGameParams(playerOneAddress, 50, 30);

const tx1: (startGameParams: StartGameParams) => GameState = (startGameParams) => {
  return Game.startGame(startGameParams).gameState;
};

const tx1GameState = tx1(startGameParams);
const tx1GameStateAsScriptData = tx1GameState.toScriptData();
console.log("##########################");
console.log(`tx1 [${tx1GameState.constructor.name}]: ${JSON.stringify(tx1GameState, null, 2)}`);
console.log(
  `tx1 scriptdata : ${tx1GameStateAsScriptData.toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema)}`
);

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

// TX 2
const playerTwoAddress = "Player2Address";
const joinGameParams = new JoinGameParams(playerTwoAddress);

const tx2: (gameState: GameState, joinGameParams: JoinGameParams) => GameState = (gamestate, joinGameParams) => {
  return Game.loadGame(gamestate).joinGame(joinGameParams).gameState;
};
const tx1GameStateFromScriptData = new GameStateFactory().fromScriptData(tx1GameStateAsScriptData);
const tx2GameState = tx2(tx1GameStateFromScriptData, joinGameParams);
const tx2GameStateAsScriptData = tx2GameState.toScriptData();
console.log("##########################");
console.log(`tx2 [${tx2GameState.constructor.name}]: ${JSON.stringify(tx2GameState, null, 2)}`);
console.log(
  `tx2 scriptdata : ${tx2GameStateAsScriptData.toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema)}`
);

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////
// TX 3
// fake game start with player 2 as O and move A 1

//    1   2   3  
// A _O_|___|___ 
// B ___|___|___
// C    |   | 


const move1 = new Move(Row.ROW_A, Column.Col_1);
const makeMoveParams1 = new MakeMoveParams(playerTwoAddress, move1);
const tx3: (gameState: GameState, makeMoveParams: MakeMoveParams) => GameState = (gamestate, makeMoveParams) => {
  //Make first move
  return Game.loadGame(gamestate).makeFirstMove(makeMoveParams).gameState;
};
const tx2GameStateFromScriptData = new GameStateFactory().fromScriptData(tx2GameStateAsScriptData);
const tx3GameState = tx3(tx2GameStateFromScriptData, makeMoveParams1);
const tx3GameStateAsScriptData = tx3GameState.toScriptData();
console.log("##########################");
console.log(`tx3 [${tx3GameState.constructor.name}]: ${JSON.stringify(tx3GameState, null, 2)}`);
console.log(
  `tx3 scriptdata : ${tx3GameStateAsScriptData.toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema)}`
);

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

// TX 4
// Player 1 Move B 2
//    1   2   3  

// A _O_|___|___ 
// B ___|_X_|___
// C    |   | 

const move2 = new Move(Row.ROW_B, Column.Col_2);
const makeMoveParams2 = new MakeMoveParams(playerOneAddress, move2);
const tx4: (gameState: GameState, makeMoveParams: MakeMoveParams) => GameState = (gamestate, makeMoveParams) => {
  return Game.loadGame(gamestate).makeMove(makeMoveParams).gameState;
};

const tx3GameStateFromScriptData = new GameStateFactory().fromScriptData(tx3GameStateAsScriptData);
const tx4GameState = tx4(tx3GameStateFromScriptData, makeMoveParams2);
const tx4GameStateAsScriptData = tx4GameState.toScriptData();
console.log("##########################");
console.log(`tx4 [${tx4GameState.constructor.name}]: ${JSON.stringify(tx4GameState, null, 2)}`);
console.log(
  `tx4 scriptdata : ${tx4GameStateAsScriptData.toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema)}`
);

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

// TX 5
// Player 2 Move A 2

// A _O_|_O_|___ 
// B ___|_X_|___
// C    |   | 

const move3 = new Move(Row.ROW_A, Column.Col_2);
const makeMoveParams3 = new MakeMoveParams(playerTwoAddress, move3);
const tx5: (gameState: GameState, makeMoveParams: MakeMoveParams) => GameState = (gamestate, makeMoveParams) => {
  return Game.loadGame(gamestate).makeMove(makeMoveParams).gameState;
};

const tx4GameStateFromScriptData = new GameStateFactory().fromScriptData(tx4GameStateAsScriptData);
const tx5GameState = tx5(tx4GameStateFromScriptData, makeMoveParams3);
const tx5GameStateAsScriptData = tx5GameState.toScriptData();
console.log("##########################");
console.log(`tx5 [${tx5GameState.constructor.name}]: ${JSON.stringify(tx5GameState, null, 2)}`);
console.log(
  `tx5 scriptdata : ${tx5GameStateAsScriptData.toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema)}`
);

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

// TX 6
// Player 1 Move B 3

// A _O_|_O_|___ 
// B ___|_X_|_X_
// C    |   | 

const move4 = new Move(Row.ROW_B, Column.Col_3);
const makeMoveParams4 = new MakeMoveParams(playerOneAddress, move4);
const tx6: (gameState: GameState, makeMoveParams: MakeMoveParams) => GameState = (gamestate, makeMoveParams) => {
  return Game.loadGame(gamestate).makeMove(makeMoveParams).gameState;
};

const tx5GameStateFromScriptData = new GameStateFactory().fromScriptData(tx5GameStateAsScriptData);
const tx6GameState = tx6(tx5GameStateFromScriptData, makeMoveParams4);
const tx6GameStateAsScriptData = tx6GameState.toScriptData();
console.log("##########################");
console.log(`tx6 [${tx6GameState.constructor.name}]: ${JSON.stringify(tx6GameState, null, 2)}`);
console.log(
  `tx6 scriptdata : ${tx6GameStateAsScriptData.toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema)}`
);

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

// TX 7
// Player 2 Move A 3

// A _O_|_O_|_O_ 
// B ___|_X_|_X_
// C    |   | 

const move5 = new Move(Row.ROW_A, Column.Col_3);
const makeMoveParams5 = new MakeMoveParams(playerTwoAddress, move5);
const tx7: (gameState: GameState, makeMoveParams: MakeMoveParams) => GameState = (gamestate, makeMoveParams) => {
  return Game.loadGame(gamestate).makeMove(makeMoveParams).gameState;
};

const tx6GameStateFromScriptData = new GameStateFactory().fromScriptData(tx6GameStateAsScriptData);
const tx7GameState = tx7(tx6GameStateFromScriptData, makeMoveParams5);
const tx7GameStateAsScriptData = tx7GameState.toScriptData();
console.log("##########################");
console.log(`tx7 [${tx7GameState.constructor.name}]: ${JSON.stringify(tx7GameState, null, 2)}`);
console.log(
  `tx7 scriptdata : ${tx7GameStateAsScriptData.toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema)}`
);

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

const tx7GameStateFromScriptData = new GameStateFactory().fromScriptData(tx7GameStateAsScriptData);
const tx8: (gameState: GameState) => void = (gamestate) => {
  Game.loadGame(gamestate).claimWin();
};
// TX 8 Claim Win
console.log("##########################");
tx8(tx7GameStateFromScriptData);
