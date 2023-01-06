import assert from "assert";


export class Tip {
  constructor(
    public readonly block: number,
    public readonly era: string,
    public readonly hash: string,
    public readonly slot: number,
    public readonly syncProgress: string
  ) {}

  static fromJson(jsonString:string):Tip{
    const jsonObject=JSON.parse(jsonString);
    assert.equal("block" in jsonObject, true);
    assert.equal(typeof jsonObject.block == "number", true);

    assert.equal("era" in jsonObject, true);
    assert.equal(typeof jsonObject.era == "string", true);

    assert.equal("hash" in jsonObject, true);
    assert.equal(typeof jsonObject.hash == "string", true);

    assert.equal("slot" in jsonObject, true);
    assert.equal(typeof jsonObject.slot == "number", true);
    
    assert.equal("syncProgress" in jsonObject, true);
    assert.equal(typeof jsonObject.syncProgress == "string", true);

    return new Tip(
      jsonObject.block,
      jsonObject.era,
      jsonObject.hash,
      jsonObject.slot,
      jsonObject.slosyncProgresst,
    )
    
  }
}
