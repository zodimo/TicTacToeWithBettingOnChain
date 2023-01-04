import { GameState, ToRedeemerScriptData } from "../../app/game-data.js";
import { createTempFilename } from "../../cardano-cli/temp-dir.js";
import fs from "fs";
import { ScriptDataJsonSchema } from "../../cardano-cli/script-data.js";

export const writeGameStateAsDatumToFile: (gameState: GameState) => string = (gameState) => {
  const UID = Math.random().toString(36).slice(2, 9);
  const tempDatumFile = createTempFilename(`datum_${UID}.json`);
  fs.writeFileSync(
    tempDatumFile,
    gameState.toScriptData().toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema)
  );
  return tempDatumFile;
};
export const writeCommandParamsAsRedeemerToFile: (command: ToRedeemerScriptData) => string = (command) => {
  const UID = Math.random().toString(36).slice(2, 9);
  const tempDatumFile = createTempFilename(`redeemer_${UID}.json`);
  fs.writeFileSync(
    tempDatumFile,
    command.toRedeemerScriptData().toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema)
  );
  return tempDatumFile;
};
