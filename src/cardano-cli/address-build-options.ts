import { PaymentComponent, StakingComponent } from "./command/address/build.js";

export class PaymentAddressBuildOptions {
  constructor(
    public readonly paymentComponent: PaymentComponent,
    public readonly stakingComponent?: StakingComponent
  ) {}
}
