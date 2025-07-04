(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_ALREADY_MEMBER (err u101))
(define-constant ERR_NOT_MEMBER (err u102))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u103))
(define-constant ERR_ALREADY_VOTED (err u104))
(define-constant ERR_VOTING_ENDED (err u105))
(define-constant ERR_INSUFFICIENT_FUNDS (err u106))
(define-constant ERR_PROPOSAL_NOT_PASSED (err u107))
(define-constant ERR_ALREADY_EXECUTED (err u108))
(define-constant ERR_INVALID_AMOUNT (err u109))
(define-constant REPUTATION_CONTRIBUTION_POINTS u10)
(define-constant REPUTATION_PROPOSAL_POINTS u25)
(define-constant REPUTATION_VOTE_POINTS u5)

(define-data-var next-proposal-id uint u1)
(define-data-var total-members uint u0)
(define-data-var treasury-balance uint u0)

(define-map members principal bool)
(define-map member-contributions principal uint)

(define-map proposals
  uint
  {
    proposer: principal,
    recipient: principal,
    amount: uint,
    title: (string-ascii 100),
    description: (string-ascii 500),
    votes-for: uint,
    votes-against: uint,
    voting-end-height: uint,
    executed: bool,
    created-at: uint
  }
)

(define-map proposal-votes
  { proposal-id: uint, voter: principal }
  { vote: bool, voted: bool }
)

(define-public (join-dao)
  (let ((caller tx-sender))
    (asserts! (is-none (map-get? members caller)) ERR_ALREADY_MEMBER)
    (map-set members caller true)
    (var-set total-members (+ (var-get total-members) u1))
    (ok true)
  )
)

(define-public (contribute (amount uint))
  (let ((caller tx-sender))
    (asserts! (is-some (map-get? members caller)) ERR_NOT_MEMBER)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount caller (as-contract tx-sender)))
    (var-set treasury-balance (+ (var-get treasury-balance) amount))
    (map-set member-contributions 
      caller 
      (+ (default-to u0 (map-get? member-contributions caller)) amount)
    )
    (ok true)
  )
)

(define-public (create-proposal 
  (recipient principal)
  (amount uint)
  (title (string-ascii 100))
  (description (string-ascii 500))
)
  (let (
    (caller tx-sender)
    (proposal-id (var-get next-proposal-id))
    (voting-period u144)
  )
    (asserts! (is-some (map-get? members caller)) ERR_NOT_MEMBER)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= amount (var-get treasury-balance)) ERR_INSUFFICIENT_FUNDS)
    
    (map-set proposals proposal-id {
      proposer: caller,
      recipient: recipient,
      amount: amount,
      title: title,
      description: description,
      votes-for: u0,
      votes-against: u0,
      voting-end-height: (+ stacks-block-height voting-period),
      executed: false,
      created-at: stacks-block-height
    })
    
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let (
    (caller tx-sender)
    (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
    (vote-key { proposal-id: proposal-id, voter: caller })
    (existing-vote (map-get? proposal-votes vote-key))
  )
    (asserts! (is-some (map-get? members caller)) ERR_NOT_MEMBER)
    (asserts! (< stacks-block-height (get voting-end-height proposal)) ERR_VOTING_ENDED)
    (asserts! (is-none existing-vote) ERR_ALREADY_VOTED)
    
    (map-set proposal-votes vote-key { vote: vote-for, voted: true })
    
    (if vote-for
      (map-set proposals proposal-id 
        (merge proposal { votes-for: (+ (get votes-for proposal) u1) })
      )
      (map-set proposals proposal-id 
        (merge proposal { votes-against: (+ (get votes-against proposal) u1) })
      )
    )
    (ok true)
  )
)

(define-public (execute-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
    (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
    (required-votes (/ (var-get total-members) u2))
  )
    (asserts! (>= stacks-block-height (get voting-end-height proposal)) ERR_VOTING_ENDED)
    (asserts! (not (get executed proposal)) ERR_ALREADY_EXECUTED)
    (asserts! (> (get votes-for proposal) (get votes-against proposal)) ERR_PROPOSAL_NOT_PASSED)
    (asserts! (>= total-votes required-votes) ERR_PROPOSAL_NOT_PASSED)
    (asserts! (>= (var-get treasury-balance) (get amount proposal)) ERR_INSUFFICIENT_FUNDS)
    
    (try! (as-contract (stx-transfer? (get amount proposal) tx-sender (get recipient proposal))))
    (var-set treasury-balance (- (var-get treasury-balance) (get amount proposal)))
    (map-set proposals proposal-id (merge proposal { executed: true }))
    (ok true)
  )
)

(define-read-only (get-member-status (member principal))
  (default-to false (map-get? members member))
)

(define-read-only (get-member-contribution (member principal))
  (default-to u0 (map-get? member-contributions member))
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? proposal-votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-treasury-balance)
  (var-get treasury-balance)
)

(define-read-only (get-total-members)
  (var-get total-members)
)

(define-read-only (get-next-proposal-id)
  (var-get next-proposal-id)
)

(define-read-only (is-proposal-passed (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal (let (
      (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
      (required-votes (/ (var-get total-members) u2))
    )
      (and 
        (>= stacks-block-height (get voting-end-height proposal))
        (> (get votes-for proposal) (get votes-against proposal))
        (>= total-votes required-votes)
      )
    )
    false
  )
)

(define-read-only (get-proposal-status (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal (let (
      (voting-active (< stacks-block-height (get voting-end-height proposal)))
      (passed (is-proposal-passed proposal-id))
      (executed (get executed proposal))
    )
      {
        voting-active: voting-active,
        passed: passed,
        executed: executed,
        votes-for: (get votes-for proposal),
        votes-against: (get votes-against proposal)
      }
    )

    {
      voting-active: false,
      passed: false,
      executed: false,
      votes-for: u0,
      votes-against: u0
    }
  )
)



(define-map member-reputation principal uint)

(define-private (award-reputation (member principal) (points uint))
  (map-set member-reputation 
    member 
    (+ (default-to u0 (map-get? member-reputation member)) points)
  )
)

(define-public (contribute-with-reputation (amount uint))
  (let ((caller tx-sender))
    (asserts! (is-some (map-get? members caller)) ERR_NOT_MEMBER)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount caller (as-contract tx-sender)))
    (var-set treasury-balance (+ (var-get treasury-balance) amount))
    (map-set member-contributions 
      caller 
      (+ (default-to u0 (map-get? member-contributions caller)) amount)
    )
    (award-reputation caller (/ amount u100000))
    (award-reputation caller REPUTATION_CONTRIBUTION_POINTS)
    (ok true)
  )
)

(define-public (create-proposal-with-reputation 
  (recipient principal)
  (amount uint)
  (title (string-ascii 100))
  (description (string-ascii 500))
)
  (let (
    (caller tx-sender)
    (proposal-id (var-get next-proposal-id))
    (voting-period u144)
  )
    (asserts! (is-some (map-get? members caller)) ERR_NOT_MEMBER)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= amount (var-get treasury-balance)) ERR_INSUFFICIENT_FUNDS)
    
    (map-set proposals proposal-id {
      proposer: caller,
      recipient: recipient,
      amount: amount,
      title: title,
      description: description,
      votes-for: u0,
      votes-against: u0,
      voting-end-height: (+ stacks-block-height voting-period),
      executed: false,
      created-at: stacks-block-height
    })
    
    (var-set next-proposal-id (+ proposal-id u1))
    (award-reputation caller REPUTATION_PROPOSAL_POINTS)
    (ok proposal-id)
  )
)

(define-public (vote-with-reputation (proposal-id uint) (vote-for bool))
  (let (
    (caller tx-sender)
    (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
    (vote-key { proposal-id: proposal-id, voter: caller })
    (existing-vote (map-get? proposal-votes vote-key))
  )
    (asserts! (is-some (map-get? members caller)) ERR_NOT_MEMBER)
    (asserts! (< stacks-block-height (get voting-end-height proposal)) ERR_VOTING_ENDED)
    (asserts! (is-none existing-vote) ERR_ALREADY_VOTED)
    
    (map-set proposal-votes vote-key { vote: vote-for, voted: true })
    
    (if vote-for
      (map-set proposals proposal-id 
        (merge proposal { votes-for: (+ (get votes-for proposal) u1) })
      )
      (map-set proposals proposal-id 
        (merge proposal { votes-against: (+ (get votes-against proposal) u1) })
      )
    )
    (award-reputation caller REPUTATION_VOTE_POINTS)
    (ok true)
  )
)

(define-read-only (get-member-reputation (member principal))
  (default-to u0 (map-get? member-reputation member))
)