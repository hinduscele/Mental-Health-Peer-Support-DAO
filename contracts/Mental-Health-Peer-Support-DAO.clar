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

(define-constant ERR_SELF_ENDORSEMENT (err u201))
(define-constant ERR_ALREADY_ENDORSED (err u202))
(define-constant ERR_ENDORSEMENT_NOT_FOUND (err u203))
(define-constant MAX_ENDORSEMENT_TEXT u200)

(define-constant ERR_INSUFFICIENT_CREDITS (err u301))
(define-constant ERR_INVALID_CREDITS (err u302))
(define-constant BASE_VOTE_CREDITS u100)
(define-constant REPUTATION_CREDIT_MULTIPLIER u10)

(define-constant ERR_INVALID_MILESTONES (err u401))
(define-constant ERR_MILESTONE_NOT_FOUND (err u402))
(define-constant ERR_MILESTONE_COMPLETED (err u403))
(define-constant ERR_NOT_VALIDATOR (err u404))
(define-constant ERR_INVALID_MILESTONE_INDEX (err u405))

(define-data-var next-milestone-proposal-id uint u1)

(define-data-var next-endorsement-id uint u1)


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

(define-map endorsements
  uint
  {
    endorser: principal,
    endorsed: principal,
    category: (string-ascii 20),
    message: (string-ascii 200),
    created-at: uint
  }
)

(define-map member-endorsements
  { endorser: principal, endorsed: principal }
  bool
)

(define-map endorsement-counts
  principal
  { received: uint, given: uint }
)

(define-map endorsement-categories
  principal
  { support: uint, guidance: uint, empathy: uint, leadership: uint }
)

(define-public (endorse-member 
  (endorsed-member principal)
  (category (string-ascii 20))
  (message (string-ascii 200))
)
  (let (
    (endorser tx-sender)
    (endorsement-id (var-get next-endorsement-id))
    (endorsement-key { endorser: endorser, endorsed: endorsed-member })
  )
    (asserts! (not (is-eq endorser endorsed-member)) ERR_SELF_ENDORSEMENT)
    (asserts! (is-none (map-get? member-endorsements endorsement-key)) ERR_ALREADY_ENDORSED)
    
    (map-set endorsements endorsement-id {
      endorser: endorser,
      endorsed: endorsed-member,
      category: category,
      message: message,
      created-at: stacks-block-height
    })
    
    (map-set member-endorsements endorsement-key true)
    (var-set next-endorsement-id (+ endorsement-id u1))
    
    (update-endorsement-counts endorsed-member endorser category)
    (ok endorsement-id)
  )
)

(define-private (update-endorsement-counts (endorsed principal) (endorser principal) (category (string-ascii 20)))
  (let (
    (endorsed-counts (default-to { received: u0, given: u0 } (map-get? endorsement-counts endorsed)))
    (endorser-counts (default-to { received: u0, given: u0 } (map-get? endorsement-counts endorser)))
    (endorsed-categories (default-to { support: u0, guidance: u0, empathy: u0, leadership: u0 } 
                         (map-get? endorsement-categories endorsed)))
  )
    (map-set endorsement-counts endorsed 
      (merge endorsed-counts { received: (+ (get received endorsed-counts) u1) }))
    (map-set endorsement-counts endorser 
      (merge endorser-counts { given: (+ (get given endorser-counts) u1) }))
    
    (map-set endorsement-categories endorsed
      (if (is-eq category "support")
        (merge endorsed-categories { support: (+ (get support endorsed-categories) u1) })
        (if (is-eq category "guidance")
          (merge endorsed-categories { guidance: (+ (get guidance endorsed-categories) u1) })
          (if (is-eq category "empathy")
            (merge endorsed-categories { empathy: (+ (get empathy endorsed-categories) u1) })
            (merge endorsed-categories { leadership: (+ (get leadership endorsed-categories) u1) })))))
  )
)

(define-read-only (get-endorsement (endorsement-id uint))
  (map-get? endorsements endorsement-id)
)

(define-read-only (get-member-endorsement-counts (member principal))
  (default-to { received: u0, given: u0 } (map-get? endorsement-counts member))
)

(define-read-only (get-member-category-endorsements (member principal))
  (default-to { support: u0, guidance: u0, empathy: u0, leadership: u0 } 
             (map-get? endorsement-categories member))
)

(define-read-only (has-endorsed (endorser principal) (endorsed principal))
  (default-to false (map-get? member-endorsements { endorser: endorser, endorsed: endorsed }))
)

(define-read-only (get-next-endorsement-id)
  (var-get next-endorsement-id)
)


(define-map quadratic-vote-credits
  { proposal-id: uint, voter: principal }
  { credits-spent: uint, vote-weight: uint, vote-direction: bool }
)

(define-public (quadratic-vote (proposal-id uint) (credits uint) (vote-for bool))
  (let (
    (caller tx-sender)
    (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
    (vote-key { proposal-id: proposal-id, voter: caller })
    (existing-qv (map-get? quadratic-vote-credits vote-key))
    (existing-vote (map-get? proposal-votes vote-key))
    (reputation (get-member-reputation caller))
    (max-credits (+ BASE_VOTE_CREDITS (/ (* reputation REPUTATION_CREDIT_MULTIPLIER) u10)))
    (vote-weight (sqrti credits))
  )
    (asserts! (is-some (map-get? members caller)) ERR_NOT_MEMBER)
    (asserts! (< stacks-block-height (get voting-end-height proposal)) ERR_VOTING_ENDED)
    (asserts! (is-none existing-qv) ERR_ALREADY_VOTED)
    (asserts! (is-none existing-vote) ERR_ALREADY_VOTED)
    (asserts! (> credits u0) ERR_INVALID_CREDITS)
    (asserts! (<= credits max-credits) ERR_INSUFFICIENT_CREDITS)
    
    (map-set quadratic-vote-credits vote-key {
      credits-spent: credits,
      vote-weight: vote-weight,
      vote-direction: vote-for
    })
    
    (map-set proposal-votes vote-key { vote: vote-for, voted: true })
    
    (if vote-for
      (map-set proposals proposal-id 
        (merge proposal { votes-for: (+ (get votes-for proposal) vote-weight) })
      )
      (map-set proposals proposal-id 
        (merge proposal { votes-against: (+ (get votes-against proposal) vote-weight) })
      )
    )
    (ok vote-weight)
  )
)

(define-read-only (get-available-vote-credits (member principal))
  (let (
    (reputation (get-member-reputation member))
  )
    (+ BASE_VOTE_CREDITS (/ (* reputation REPUTATION_CREDIT_MULTIPLIER) u10))
  )
)

(define-read-only (get-quadratic-vote-info (proposal-id uint) (voter principal))
  (map-get? quadratic-vote-credits { proposal-id: proposal-id, voter: voter })
)

(define-read-only (calculate-vote-weight (credits uint))
  (sqrti credits)
)


(define-map milestone-proposals
  uint
  {
    proposer: principal,
    recipient: principal,
    total-amount: uint,
    title: (string-ascii 100),
    milestone-count: uint,
    completed-milestones: uint,
    current-released: uint,
    created-at: uint,
    active: bool
  }
)

(define-map milestones
  { proposal-id: uint, milestone-index: uint }
  {
    description: (string-ascii 300),
    amount: uint,
    completed: bool,
    validator: principal,
    verified-at: uint
  }
)

(define-public (create-milestone-proposal
  (recipient principal)
  (title (string-ascii 100))
  (milestone-descriptions (list 5 (string-ascii 300)))
  (milestone-amounts (list 5 uint))
)
  (let (
    (caller tx-sender)
    (proposal-id (var-get next-milestone-proposal-id))
    (milestone-count (len milestone-descriptions))
    (total-amount (fold + milestone-amounts u0))
  )
    (asserts! (is-some (map-get? members caller)) ERR_NOT_MEMBER)
    (asserts! (> milestone-count u0) ERR_INVALID_MILESTONES)
    (asserts! (is-eq (len milestone-amounts) milestone-count) ERR_INVALID_MILESTONES)
    (asserts! (<= total-amount (var-get treasury-balance)) ERR_INSUFFICIENT_FUNDS)
    
    (map-set milestone-proposals proposal-id {
      proposer: caller,
      recipient: recipient,
      total-amount: total-amount,
      title: title,
      milestone-count: milestone-count,
      completed-milestones: u0,
      current-released: u0,
      created-at: stacks-block-height,
      active: true
    })
    
    (var-set next-milestone-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

(define-public (verify-milestone (proposal-id uint) (milestone-index uint))
  (let (
    (caller tx-sender)
    (proposal (unwrap! (map-get? milestone-proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
    (milestone-key { proposal-id: proposal-id, milestone-index: milestone-index })
    (milestone (unwrap! (map-get? milestones milestone-key) ERR_MILESTONE_NOT_FOUND))
  )
    (asserts! (is-some (map-get? members caller)) ERR_NOT_MEMBER)
    (asserts! (get active proposal) ERR_PROPOSAL_NOT_FOUND)
    (asserts! (not (get completed milestone)) ERR_MILESTONE_COMPLETED)
    
    (map-set milestones milestone-key
      (merge milestone { completed: true, validator: caller, verified-at: stacks-block-height })
    )
    
    (try! (as-contract (stx-transfer? (get amount milestone) tx-sender (get recipient proposal))))
    
    (let ((new-completed (+ (get completed-milestones proposal) u1))
          (new-released (+ (get current-released proposal) (get amount milestone))))
      (map-set milestone-proposals proposal-id
        (merge proposal {
          completed-milestones: new-completed,
          current-released: new-released,
          active: (< new-completed (get milestone-count proposal))
        })
      )
      (var-set treasury-balance (- (var-get treasury-balance) (get amount milestone)))
      (ok true)
    )
  )
)

(define-read-only (get-milestone-proposal (proposal-id uint))
  (map-get? milestone-proposals proposal-id)
)

(define-read-only (get-milestone (proposal-id uint) (milestone-index uint))
  (map-get? milestones { proposal-id: proposal-id, milestone-index: milestone-index })
)