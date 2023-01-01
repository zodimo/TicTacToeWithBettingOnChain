import { UtxoId } from "../cardano-cli/utxo-id.js";
import { cardanoCli } from "../previewCardanoCliJs.js";
import { getScriptAddress } from "../smart-contract.js";
import { fromJson } from "../cardano-cli/script-data.js";
import { StartGameData } from "./emurgo-datum.js";

const scriptAddress = getScriptAddress();
const utxoIdWithDatum = UtxoId.fromString(
  "7a483c918811971f1ca10cd4949b2bb0fe5da879d8a4211d8af5d727044974af#1" //working
);

const utxoStack = cardanoCli.getUtxoStackForAddress(scriptAddress);

utxoStack.utxos.map((utxo) => {
  if (utxo.id.equals(utxoIdWithDatum)) {
    if (utxo.inlineDatum) {
      const inlineDatumAsJson = JSON.stringify(utxo.inlineDatum);
      const startGameData = StartGameData.fromScriptData(fromJson(inlineDatumAsJson));
      console.log(startGameData);
    }
  }
});
