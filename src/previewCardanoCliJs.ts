import { CardanoCli, CardanoCliOptions } from "./cardano-cli/cardano-cli.js";
import { config } from "./cardano-cli-config.js";

const cardanoCliOptions = new CardanoCliOptions(
  config.getWorkingDirectory(),
  config.getEra(),
  config.getNetwork()
);
cardanoCliOptions.debug = config.getDebug();

export const cardanoCli = new CardanoCli(cardanoCliOptions);
