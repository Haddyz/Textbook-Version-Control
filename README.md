# 📚 Textbook Version Control

A revolutionary blockchain-based textbook distribution system built on Stacks blockchain where authors can publish updates and students automatically receive revisions through NFT-based ownership.

## 🚀 Features

- **📖 NFT-Based Textbooks**: Each textbook is represented as a unique NFT
- **🔄 Version Control**: Authors can publish updates with detailed changelogs
- **👥 Student Subscriptions**: Automated update delivery to subscribed students
- **💰 Revenue Tracking**: Built-in payment system for textbook access
- **📊 Analytics**: Track downloads, subscribers, and earnings
- **⚙️ Auto-Update Settings**: Students can choose automatic or manual updates

## 🏗️ Smart Contract Architecture

### Core Components

- **Textbook NFTs**: Unique tokens representing textbook ownership
- **Version Management**: Track multiple versions with content hashes
- **Subscription System**: Manage student access and payment
- **Analytics Engine**: Monitor usage and revenue metrics

### Key Data Structures

- `textbooks`: Main textbook metadata and author information
- `textbook-versions`: Version-specific content and changelogs
- `student-subscriptions`: Student access rights and sync status
- `textbook-stats`: Usage analytics and revenue tracking

## 🛠️ Usage Instructions

### For Authors 👨‍🏫

#### 1. Create a New Textbook
```clarity
(contract-call? .textbook-version-control create-textbook 
  "Advanced Blockchain Development" 
  "Comprehensive guide to smart contract development"
  "QmHash123..." 
  u5000000)
```

#### 2. Publish Updates
```clarity
(contract-call? .textbook-version-control publish-update 
  u1 
  "QmNewHash456..." 
  "Added chapter on DeFi protocols" 
  u2048576)
```

#### 3. Check Earnings
```clarity
(contract-call? .textbook-version-control get-author-earnings tx-sender)
```

### For Students 🎓

#### 1. Subscribe to a Textbook
```clarity
(contract-call? .textbook-version-control subscribe-to-textbook u1 true)
```

#### 2. Sync Latest Version
```clarity
(contract-call? .textbook-version-control sync-textbook u1)
```

#### 3. Check for Pending Updates
```clarity
(contract-call? .textbook-version-control get-pending-updates tx-sender u1)
```

#### 4. Toggle Auto-Updates
```clarity
(contract-call? .textbook-version-control toggle-auto-update u1)
```

## 📋 Function Reference

### Public Functions

| Function | Description | Parameters |
|----------|-------------|------------|
| `create-textbook` | Create new textbook NFT | title, description, content-hash, price |
| `publish-update` | Release new version | textbook-id, content-hash, changelog, size |
| `subscribe-to-textbook` | Subscribe and pay for access | textbook-id, auto-update |
| `sync-textbook` | Download latest version | textbook-id |
| `unsubscribe-from-textbook` | Cancel subscription | textbook-id |
| `toggle-auto-update` | Change update preferences | textbook-id |

### Read-Only Functions

| Function | Description | Returns |
|----------|-------------|---------|
| `get-textbook` | Get textbook metadata | textbook info |
| `get-textbook-version` | Get specific version data | version details |
| `get-subscription` | Check subscription status | subscription info |
| `get-pending-updates` | Count pending updates | number of updates |
| `is-subscribed` | Verify active subscription | boolean |

## 🔧 Development Setup

### Prerequisites
- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Node.js and npm

### Installation
```bash
# Clone the repository
git clone https://github.com/your-username/textbook-version-control
cd textbook-version-control

# Install dependencies
npm install

# Check contract syntax
clarinet check
```

### Testing
```bash
# Run all tests
npm test

# Run specific test file
npm test tests/textbook_test.ts
```

## 💡 Example Workflow

1. **Author creates textbook**: `create-textbook("Blockchain 101", ...)`
2. **Student subscribes**: `subscribe-to-textbook(1, true)`
3. **Author publishes update**: `publish-update(1, "new-hash", "Added DeFi chapter")`
4. **Student syncs automatically**: `sync-textbook(1)` (if auto-update enabled)
5. **Revenue flows to author**: Automatic STX transfer on subscription

## 🔒 Security Features

- ✅ Author-only update permissions
- ✅ Payment verification before access
- ✅ Subscription status validation
- ✅ Version integrity through content hashes

## 📈 Economics

- **Subscription Model**: One-time payment for lifetime access
- **Revenue Sharing**: 100% to authors (minus network fees)
- **Update Incentives**: Authors earn from initial subscriptions

## 🤝 Contributing

1. Fork the repository
2. Create feature branch: `git checkout -b feature/amazing-feature`
3. Commit changes: `git commit -m 'Add amazing feature'`
4. Push to branch: `git push origin feature/amazing-feature`
5. Open a Pull Request

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🆘 Support

- 📧 Email: support@textbook-vc.com
- 💬 Discord: [Join our community](https://discord.gg/textbook-vc)
- 📖 Documentation: [Full docs](https://docs.textbook-vc.com)

---

*Built with ❤️ on the Stacks blockchain*
