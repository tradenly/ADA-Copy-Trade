# ADA-Copy-Trade
# Copy Trading Contract

## Components

### CopyTradingParams
Defines the wallet address to copy, maximum buy amount, and slippage tolerance.

### CopyTradingDatum
Stores the trader's public key hash, copied wallet address, maximum amount, and slippage.

### CopyTradingRedeemer
Defines the actions (**CopyTrade** or **UpdateParams**) that can be performed on the contract.

### copyTradingValidator
Contains the logic to validate copy trades and parameter updates.

### Helper Functions
Functions to fetch transactions, calculate trade amounts, and adjust for slippage.
