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

(define-constant ACTIVITY_DECAY_PERIOD u1008)
(define-constant ACTIVITY_CONTRIBUTION_WEIGHT u3)
(define-constant ACTIVITY_PROPOSAL_WEIGHT u5)
(define-constant ACTIVITY_VOTE_WEIGHT u2)
(define-constant ACTIVITY_JOIN_WEIGHT u1)

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


(define-map member-last-activity principal uint)
(define-map member-activity-score principal uint)
(define-map member-activity-history principal { contributions: uint, proposals: uint, votes: uint, last-updated: uint })

(define-private (update-member-activity (member principal) (activity-type (string-ascii 20)))
  (let (
    (current-height stacks-block-height)
    (last-activity (default-to u0 (map-get? member-last-activity member)))
    (current-score (default-to u0 (map-get? member-activity-score member)))
    (history (default-to { contributions: u0, proposals: u0, votes: u0, last-updated: u0 } 
             (map-get? member-activity-history member)))
    (time-diff (- current-height last-activity))
    (decay-factor (if (> time-diff ACTIVITY_DECAY_PERIOD) u2 u1))
    (activity-points (if (is-eq activity-type "join") ACTIVITY_JOIN_WEIGHT
                     (if (is-eq activity-type "contribute") ACTIVITY_CONTRIBUTION_WEIGHT
                     (if (is-eq activity-type "propose") ACTIVITY_PROPOSAL_WEIGHT
                     (if (is-eq activity-type "vote") ACTIVITY_VOTE_WEIGHT u0)))))
    (decayed-score (/ current-score decay-factor))
    (new-score (+ decayed-score activity-points))
    (updated-history (if (is-eq activity-type "contribute")
                      (merge history { contributions: (+ (get contributions history) u1), last-updated: current-height })
                      (if (is-eq activity-type "propose")
                       (merge history { proposals: (+ (get proposals history) u1), last-updated: current-height })
                       (if (is-eq activity-type "vote")
                        (merge history { votes: (+ (get votes history) u1), last-updated: current-height })
                        (merge history { last-updated: current-height })))))
  )
    (map-set member-last-activity member current-height)
    (map-set member-activity-score member new-score)
    (map-set member-activity-history member updated-history)
    new-score
  )
)

(define-read-only (get-member-activity-score (member principal))
  (let (
    (current-height stacks-block-height)
    (last-activity (default-to u0 (map-get? member-last-activity member)))
    (current-score (default-to u0 (map-get? member-activity-score member)))
    (time-diff (- current-height last-activity))
    (decay-factor (if (> time-diff ACTIVITY_DECAY_PERIOD) u2 u1))
  )
    (/ current-score decay-factor)
  )
)

(define-read-only (get-member-activity-history (member principal))
  (default-to { contributions: u0, proposals: u0, votes: u0, last-updated: u0 } 
             (map-get? member-activity-history member))
)

(define-read-only (get-active-members-count)
  (var-get total-members)
)