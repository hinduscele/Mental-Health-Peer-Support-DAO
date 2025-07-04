# 🧠 Mental Health Peer Support DAO

A decentralized autonomous organization focused on providing peer support and funding mental health resources through community governance.

## 🌟 Features

- 👥 **Member Management**: Join the DAO and become part of the mental health support community
- 💰 **Treasury System**: Members contribute STX to fund mental health initiatives
- 📝 **Proposal Creation**: Submit funding requests for mental health resources and support
- 🗳️ **Democratic Voting**: Vote on proposals to allocate community funds
- ⚡ **Automatic Execution**: Approved proposals are automatically executed

## 🚀 Getting Started

### Prerequisites
- Clarinet CLI installed
- Stacks wallet for testing

### Installation

```bash
git clone <repository-url>
cd mental-health-dao
clarinet check
```

## 📖 Usage

### 1. Join the DAO
```clarity
(contract-call? .mental-health-dao join-dao)
```

### 2. Contribute to Treasury
```clarity
(contract-call? .mental-health-dao contribute u1000000)
```

### 3. Create a Proposal
```clarity
(contract-call? .mental-health-dao create-proposal 
  'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM
  u500000
  "Therapy Session Funding"
  "Request funding for group therapy sessions for community members dealing with anxiety")
```

### 4. Vote on Proposals
```clarity
(contract-call? .mental-health-dao vote-on-proposal u1 true)
```

### 5. Execute Approved Proposals
```clarity
(contract-call? .mental-health-dao execute-proposal u1)
```

## 🔍 Read-Only Functions

### Check Member Status
```clarity
(contract-call? .mental-health-dao get-member-status 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

### View Treasury Balance
```clarity
(contract-call? .mental-health-dao get-treasury-balance)
```

### Get Proposal Details
```clarity
(contract-call? .mental-health-dao get-proposal u1)
```

### Check Proposal Status
```clarity
(contract-call? .mental-health-dao get-proposal-status u1)
```

## 🏛️ Governance Rules

- **Membership**: Open to anyone who calls `join-dao`
- **Voting Period**: 144 blocks (~24 hours)
- **Quorum**: At least 50% of members must vote
- **Approval**: Simple majority (more votes for than against)
- **Execution**: Automatic after voting period ends for passed proposals

## 🛡️ Security Features

- ✅ Member-only proposal creation and voting
- ✅ One vote per member per proposal
- ✅ Automatic fund management through contract escrow
- ✅ Proposal execution only after approval
- ✅ Treasury balance validation

## 🤝 Contributing

This is a mental health support community project. Contributions that improve accessibility, security, and user experience are welcome.

## 📄 License

MIT License - Built for the mental health community with ❤️

## 🆘 Support

If you're struggling with mental health, please reach out to local mental health services or crisis hotlines. This DAO is meant to supplement, not replace, professional mental health care.
```

**Git Commit Message:**
```
feat: implement mental health peer support DAO with voting and treasury management
```

**GitHub Pull Request Title:**
```
🧠 Add Mental Health Peer Support DAO MVP
```

**GitHub Pull Request Description:**
```
## 🎯 Summary
Implements a complete Mental Health Peer Support DAO that enables community members to collectively fund mental health resources and support initiatives through democratic governance.

## ✨ Features Added
- Member registration and management system
- Community treasury with STX contributions
- Proposal creation for funding requests
- Democratic voting mechanism with quorum requirements
- Automatic execution of approved proposals
- Comprehensive read-only functions for transparency

## 🏗️ Technical Implementation
- 150+ lines of clean Clarity smart contract code
- Secure fund management with contract escrow
- Voting period of 144 blocks (~24 hours)
- Simple majority voting with 50% quorum requirement
- Error handling for all edge cases

## 📚 Documentation
- Complete README with usage examples
- Clear function documentation and governance rules
- Setup instructions for local development

This MVP provides a solid foundation for a mental health support community to self-organize and allocate resources democratically.
