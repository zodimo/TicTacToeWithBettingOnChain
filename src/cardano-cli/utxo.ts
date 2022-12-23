export class UtxoNativeAsset{
    constructor(
        public readonly policyId:string,
        public readonly assetName:string,
        public readonly quantity:number,
    ){
    }
}

export class UtxoValue{
    constructor(
        public readonly lovelace:number,
        public readonly nativeAssets:UtxoNativeAsset[]
    ){
        // { lovelace: 9389640398 }
    }
}

export class Utxo{
    constructor(
        public readonly id:string,
        public readonly address:string,
        public readonly value:UtxoValue, 
        public readonly datum?:string,        
        public readonly datumHash?:string,
        public readonly inlineDatum?:string,
        public readonly referenceScript?:string,
    ){}
}