# Script Data Json

## ScriptDataJsonDetailedSchema in cardano-node.

This is the format used by --script-data-file in cardano-cli
This covers almost all (only minor exceptions) Plutus datums, but the JSON must conform to a strict schema.
The schema specifies that ALL keys and ALL values must be contained in a JSON map with 2 cases:

1.  For ConstrPlutusData there must be two fields "constructor" contianing a number and "fields" containing its fields
    e.g. { "constructor": 2, "fields": [{"int": 2}, {"list": [{"bytes": "CAFEF00D"}]}]}
2.  For all other cases there must be only one field named "int", "bytes", "list" or "map"
    Integer's value is a JSON number e.g. {"int": 100}
    Bytes' value is a hex string representing the bytes WITHOUT any prefix e.g. {"bytes": "CAFEF00D"}
    Lists' value is a JSON list of its elements encoded via the same schema e.g. {"list": [{"bytes": "CAFEF00D"}]}
    Maps' value is a JSON list of objects, one for each key-value pair in the map, with keys "k" and "v"
    respectively with their values being the plutus datum encoded via this same schema
    e.g. {"map": [
    {"k": {"int": 2}, "v": {"int": 5}},
    {"k": {"map": [{"k": {"list": [{"int": 1}]}, "v": {"bytes": "FF03"}}]}, "v": {"list": []}}
    ]}
    From JSON:

- null/true/false/floats NOT supported
- the JSON must conform to a very specific schema
  To JSON:
- all Plutus datums should be fully supported outside of the integer range limitations outlined above.
