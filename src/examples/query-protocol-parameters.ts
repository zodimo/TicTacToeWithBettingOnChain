import { cardanoCli } from "../previewCardanoCliJs.js"
console.log(cardanoCli.query().protocolParameters().runCommand());
