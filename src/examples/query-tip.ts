import { cardanoCli } from "../previewCardanoCliJs.js"
console.log(cardanoCli.query().tip().runCommand());