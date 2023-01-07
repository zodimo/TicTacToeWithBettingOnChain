# Documentation [ Just a collection of thoughts ]

How to align the offfline application with the online validator expectations.
With the expectations of the online validator being met, the consumption of the utxos is impossible.
Documentation ?  
Api ?

- an address cannot stop it being used as destination of funds.
- as the script owner I do not want to burn any funds.
- register the address of the root wallet, to access funds not associted with primary cause,  
  where the datums/requirements do no align.

## Game state derivation process

- funds transderred to script address resulting in invalid game state can only be unlocked with root wallet.
- funds locked in the script can only be claimed with 30 days by emailing me at zodimo@gmail.com. or going to the website, unlockmyfinds.com.
- keep a list of script address iterations for people to find the script intentions via google search and request unlock if required.

- if you transafer funds without datum, it is invalid and can be unlocked with address only.
- if you transfer function not matching the requirement of the datum

## Transaction Contruction

### TX1

Inputs:

- wallet1 1 payment address + verification address
- data: deadline

# Resources

- [cardano-components/cardano-serialization-lib](https://docs.cardano.org/cardano-components/cardano-serialization-lib)
- [cardano-serialization-lib/doc/getting-started](https://github.com/Emurgo/cardano-serialization-lib/tree/master/doc/getting-started)
