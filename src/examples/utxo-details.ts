import { UtxoId, UtxoStack } from "../cardano-cli/utxo.js";
import { cardanoCli } from "../previewCardanoCliJs.js";
import { getScriptAddress } from "../smart-contract.js";
import * as a from "@emurgo/cardano-serialization-lib-nodejs";

const scriptAddress = getScriptAddress();
const utxoIdWithDatum = UtxoId.fromString(
  "0b1f727faf5804730933617782cd69e877e57a9e2f594d958b51d3de52b35133#1"
);

const utxoStack = cardanoCli.getUtxoStackFor(scriptAddress);

utxoStack.utxos.map((utxo) => {
  if (utxo.id.equals(utxoIdWithDatum)) {
    if (utxo.inlineDatum) {
      console.log(utxo.inlineDatum);
      console.log(typeof utxo.inlineDatum);
      // const inlineDatum=JSON.parse(utxo.inlineDatum);
      // console.log(inlineDatum);
      // console.log(a.PlutusData.from_json(utxo.inlineDatum,a.PlutusDatumSchema.DetailedSchema).to_json(a.PlutusDatumSchema.DetailedSchema))
    }
  }
});
