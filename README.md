# 🧪 Reproducible Research Verification NFTs (RRV) 

## 🎯 Overview
RRV is a Clarity smart contract that enables researchers to mint NFTs containing verifiable research data, methodologies, and results. It creates an immutable link between datasets and signed attestations on the Stacks blockchain.

## ✨ Features
- 🔒 Mint NFTs with research data, methodology, and result hashes
- ✅ Verification system for peer review
- 🔗 IPFS integration for data storage
- 📊 Track research provenance
- 🔄 Transfer ownership of research NFTs

## 🚀 Usage

### Minting Research NFTs
```clarity
(contract-call? .rrv mint "QmHash..." "Research methodology..." "ResultsHash...")
```

### Verifying Research
```clarity
(contract-call? .rrv verify-research u1)
```

### Checking Verification Status
```clarity
(contract-call? .rrv get-verification-status u1 tx-sender)
```

### Transferring NFTs
```clarity
(contract-call? .rrv transfer u1 sender-address recipient-address)
```

## 🛠 Development
Built with Clarinet and Clarity for the Stacks blockchain.

## 📜 License
MIT
```
