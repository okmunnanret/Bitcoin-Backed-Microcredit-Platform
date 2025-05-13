# 🏦 Bitcoin-Backed Microcredit Platform

A decentralized peer-to-peer microlending platform built on Stacks, enabling Bitcoin-backed loans for the underbanked.

## 🌟 Features

- Create loans with BTC collateral
- Fund loans as a lender
- Transparent interest rates and terms
- Automated repayment tracking
- Decentralized credit scoring

## 🚀 Quick Start

1. Deploy the contract using Clarinet:
```bash
clarinet deploy
```

2. Create a loan:
```bash
clarinet contract-call .microcredit-platform create-loan u1000 u1500 u500 u30
```

3. Fund a loan:
```bash
clarinet contract-call .microcredit-platform fund-loan u1
```

4. Repay a loan:
```bash
clarinet contract-call .microcredit-platform repay-loan u1 u100
```

## 📊 Contract Functions

- `create-loan`: Create a new loan request
- `fund-loan`: Fund an existing loan request
- `repay-loan`: Make loan repayments
- `get-loan`: View loan details
- `get-credit-score`: Check user's credit score

## 💡 Parameters

- Minimum collateral ratio: 150%
- Platform fee: 2.5%
- Interest rates: Set by lenders
- Loan duration: Flexible terms

## 🔒 Security

All funds are secured by smart contracts with no admin privileges.
```

