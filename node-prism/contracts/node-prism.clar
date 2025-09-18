;; Node Prism - Multi-chain Yield Farming Protocol

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-PAUSED (err u104))
(define-constant ERR-UNAUTHORIZED (err u105))

;; Data Variables
(define-data-var contract-paused bool false)
(define-data-var total-tvl uint u0)
(define-data-var base-fee uint u100) ;; 1% in basis points
(define-data-var harvest-frequency uint u21600) ;; 6 hours in seconds

;; Data Maps
(define-map user-positions 
  principal 
  {
    staked-amount: uint,
    last-harvest: uint,
    lock-period: uint,
    yield-earned: uint,
    entry-block: uint
  }
)

(define-map yield-strategies 
  uint 
  {
    name: (string-ascii 50),
    apy: uint, ;; APY in basis points (10000 = 100%)
    risk-score: uint, ;; 1-10 scale
    tvl: uint,
    active: bool
  }
)

(define-map governance-proposals 
  uint 
  {
    proposer: principal,
    description: (string-ascii 500),
    votes-for: uint,
    votes-against: uint,
    end-block: uint,
    executed: bool
  }
)

;; Token balances (simplified SIP-010 compatible)
(define-map token-balances {token: (string-ascii 10), owner: principal} uint)

;; Proposal counter
(define-data-var proposal-counter uint u0)

;; Read-only functions
(define-read-only (get-user-position (user principal))
  (map-get? user-positions user)
)

(define-read-only (get-yield-strategy (strategy-id uint))
  (map-get? yield-strategies strategy-id)
)

(define-read-only (get-total-tvl)
  (var-get total-tvl)
)

(define-read-only (get-contract-paused)
  (var-get contract-paused)
)

(define-read-only (calculate-yield (user principal))
  (let ((position (unwrap! (get-user-position user) u0)))
    (let ((blocks-passed (- block-height (get entry-block position)))
          (staked (get staked-amount position)))
      ;; Simplified yield calculation: 10% APY approximation
      (/ (* staked blocks-passed) u52560) ;; Rough blocks per year
    )
  )
)

(define-read-only (get-token-balance (token (string-ascii 10)) (owner principal))
  (default-to u0 (map-get? token-balances {token: token, owner: owner}))
)

;; Private functions
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT-OWNER)
)

(define-private (update-token-balance (token (string-ascii 10)) (owner principal) (amount uint))
  (map-set token-balances {token: token, owner: owner} amount)
)

;; Public functions

;; Governance: Create proposal
(define-public (create-proposal (description (string-ascii 500)))
  (let ((proposal-id (+ (var-get proposal-counter) u1)))
    (begin
      (map-set governance-proposals proposal-id {
        proposer: tx-sender,
        description: description,
        votes-for: u0,
        votes-against: u0,
        end-block: (+ block-height u1440), ;; ~10 days
        executed: false
      })
      (var-set proposal-counter proposal-id)
      (ok proposal-id)
    )
  )
)

;; Governance: Vote on proposal
(define-public (vote-proposal (proposal-id uint) (vote-for bool))
  (let ((proposal (unwrap! (map-get? governance-proposals proposal-id) ERR-NOT-FOUND))
        (user-position (unwrap! (get-user-position tx-sender) ERR-UNAUTHORIZED)))
    (let ((voting-power (get staked-amount user-position)))
      (if (< block-height (get end-block proposal))
        (begin
          (if vote-for
            (map-set governance-proposals proposal-id 
              (merge proposal {votes-for: (+ (get votes-for proposal) voting-power)}))
            (map-set governance-proposals proposal-id 
              (merge proposal {votes-against: (+ (get votes-against proposal) voting-power)}))
          )
          (ok true)
        )
        ERR-NOT-FOUND
      )
    )
  )
)

;; Stake tokens into yield farming
(define-public (stake-tokens (amount uint) (lock-period uint))
  (begin
    (asserts! (not (var-get contract-paused)) ERR-PAUSED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    (let ((current-position (default-to {
            staked-amount: u0,
            last-harvest: block-height,
            lock-period: u0,
            yield-earned: u0,
            entry-block: block-height
          } (get-user-position tx-sender))))
      
      ;; Update user position
      (map-set user-positions tx-sender {
        staked-amount: (+ (get staked-amount current-position) amount),
        last-harvest: block-height,
        lock-period: lock-period,
        yield-earned: (get yield-earned current-position),
        entry-block: (get entry-block current-position)
      })
      
      ;; Update total TVL
      (var-set total-tvl (+ (var-get total-tvl) amount))
      
      ;; Update PRISM token balance (governance token)
      (let ((current-prism (get-token-balance "PRISM" tx-sender)))
        (update-token-balance "PRISM" tx-sender (+ current-prism (/ amount u10)))
      )
      
      ;; Issue SHARD tokens (yield-bearing positions)
      (let ((current-shard (get-token-balance "SHARD" tx-sender)))
        (update-token-balance "SHARD" tx-sender (+ current-shard amount))
      )
      
      (ok amount)
    )
  )
)

;; Harvest yield rewards
(define-public (harvest-yield)
  (let ((position (unwrap! (get-user-position tx-sender) ERR-NOT-FOUND)))
    (let ((yield-amount (calculate-yield tx-sender))
          (blocks-since-harvest (- block-height (get last-harvest position))))
      
      (asserts! (>= blocks-since-harvest (var-get harvest-frequency)) ERR-UNAUTHORIZED)
      
      ;; Update position with harvested yield
      (map-set user-positions tx-sender 
        (merge position {
          last-harvest: block-height,
          yield-earned: (+ (get yield-earned position) yield-amount)
        }))
      
      ;; Issue BEAM tokens as liquidity rewards
      (let ((current-beam (get-token-balance "BEAM" tx-sender)))
        (update-token-balance "BEAM" tx-sender (+ current-beam (/ yield-amount u5)))
      )
      
      (ok yield-amount)
    )
  )
)

;; Unstake tokens (with lock period check)
(define-public (unstake-tokens (amount uint))
  (let ((position (unwrap! (get-user-position tx-sender) ERR-NOT-FOUND)))
    (let ((unlock-block (+ (get entry-block position) (get lock-period position))))
      
      (asserts! (>= block-height unlock-block) ERR-UNAUTHORIZED)
      (asserts! (>= (get staked-amount position) amount) ERR-INSUFFICIENT-BALANCE)
      
      ;; Update position
      (map-set user-positions tx-sender 
        (merge position {staked-amount: (- (get staked-amount position) amount)}))
      
      ;; Update total TVL
      (var-set total-tvl (- (var-get total-tvl) amount))
      
      ;; Burn SHARD tokens
      (let ((current-shard (get-token-balance "SHARD" tx-sender)))
        (update-token-balance "SHARD" tx-sender (- current-shard amount))
      )
      
      (ok amount)
    )
  )
)

;; Add new yield strategy (owner only)
(define-public (add-yield-strategy (strategy-id uint) (name (string-ascii 50)) (apy uint) (risk-score uint))
  (begin
    (asserts! (is-contract-owner) ERR-OWNER-ONLY)
    
    (map-set yield-strategies strategy-id {
      name: name,
      apy: apy,
      risk-score: risk-score,
      tvl: u0,
      active: true
    })
    
    (ok strategy-id)
  )
)

;; Emergency pause (owner only)
(define-public (pause-contract (paused bool))
  (begin
    (asserts! (is-contract-owner) ERR-OWNER-ONLY)
    (var-set contract-paused paused)
    (ok paused)
  )
)

;; Update harvest frequency (governance)
(define-public (update-harvest-frequency (new-frequency uint))
  (begin
    (asserts! (is-contract-owner) ERR-OWNER-ONLY)
    (var-set harvest-frequency new-frequency)
    (ok new-frequency)
  )
)

;; Initialize default yield strategies
(define-public (initialize-strategies)
  (begin
    (asserts! (is-contract-owner) ERR-OWNER-ONLY)
    
    ;; Add default strategies
    (try! (add-yield-strategy u1 "Compound Strategy" u1200 u3)) ;; 12% APY, risk 3
    (try! (add-yield-strategy u2 "Aave Strategy" u800 u2))     ;; 8% APY, risk 2  
    (try! (add-yield-strategy u3 "Curve Strategy" u1500 u4))   ;; 15% APY, risk 4
    
    (ok true)
  )
)