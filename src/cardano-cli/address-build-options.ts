import { PaymentComponent, StakeComponent } from "./command/address/build.js";

export class PaymentAddressBuildOptions {
  constructor(
    public readonly paymentComponent: PaymentComponent,
    public readonly stakeComponent?: StakeComponent
  ) {}
}
