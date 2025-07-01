(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-loan-exists (err u104))
(define-constant err-loan-not-active (err u105))
(define-constant err-insufficient-collateral (err u106))

(define-data-var min-collateral-ratio uint u150)
(define-data-var platform-fee uint u25)

(define-map loans
    { loan-id: uint }
    {
        borrower: principal,
        lender: (optional principal),
        amount: uint,
        collateral: uint,
        interest-rate: uint,
        duration: uint,
        status: (string-ascii 20),
        start-height: uint,
        repaid-amount: uint,
    }
)

(define-map credit-scores
    { user: principal }
    {
        score: uint,
        loans-taken: uint,
        loans-repaid: uint,
    }
)

(define-public (create-loan
        (amount uint)
        (collateral uint)
        (interest-rate uint)
        (duration uint)
    )
    (let (
            (loan-id (+ (var-get next-loan-id) u1))
            (collateral-ratio (/ (* collateral u100) amount))
        )
        (asserts! (>= collateral-ratio (var-get min-collateral-ratio))
            err-insufficient-collateral
        )
        (try! (stx-transfer? collateral tx-sender (as-contract tx-sender)))
        (map-set loans { loan-id: loan-id } {
            borrower: tx-sender,
            lender: none,
            amount: amount,
            collateral: collateral,
            interest-rate: interest-rate,
            duration: duration,
            status: "PENDING",
            start-height: u0,
            repaid-amount: u0,
        })
        (var-set next-loan-id loan-id)
        (ok loan-id)
    )
)

(define-public (fund-loan (loan-id uint))
    (let (
            (loan (unwrap! (map-get? loans { loan-id: loan-id }) err-not-found))
            (amount (get amount loan))
        )
        (asserts! (is-eq (get status loan) "PENDING") err-loan-not-active)
        (try! (stx-transfer? amount tx-sender (get borrower loan)))
        (map-set loans { loan-id: loan-id }
            (merge loan {
                lender: (some tx-sender),
                status: "ACTIVE",
                start-height: stacks-block-height,
            })
        )
        (ok true)
    )
)

(define-public (repay-loan
        (loan-id uint)
        (payment uint)
    )
    (let (
            (loan (unwrap! (map-get? loans { loan-id: loan-id }) err-not-found))
            (loan-for-interest {
                borrower: (get borrower loan),
                amount: (get amount loan),
                interest-rate: (get interest-rate loan),
                duration: (get duration loan),
                start-height: (get start-height loan),
            })
            (remaining (- (+ (get amount loan) (calculate-interest loan-for-interest))
                (get repaid-amount loan)
            ))
        )
        (asserts! (is-eq (get borrower loan) tx-sender) err-unauthorized)
        (asserts! (is-eq (get status loan) "ACTIVE") err-loan-not-active)
        (asserts! (<= payment remaining) err-invalid-amount)
        (try! (stx-transfer? payment tx-sender
            (unwrap! (get lender loan) err-not-found)
        ))
        (map-set loans { loan-id: loan-id }
            (merge loan {
                repaid-amount: (+ (get repaid-amount loan) payment),
                status: (if (is-eq payment remaining)
                    "COMPLETED"
                    "ACTIVE"
                ),
            })
        )
        (ok true)
    )
)

(define-private (calculate-interest (loan {
    borrower: principal,
    amount: uint,
    interest-rate: uint,
    duration: uint,
    start-height: uint,
}))
    (let (
            (periods-elapsed (/ (- stacks-block-height (get start-height loan)) u144))
            (rate-per-period (/ (get interest-rate loan) u10000))
        )
        (* (get amount loan) rate-per-period periods-elapsed)
    )
)

(define-data-var next-loan-id uint u0)

(define-read-only (get-loan (loan-id uint))
    (ok (unwrap! (map-get? loans { loan-id: loan-id }) err-not-found))
)

(define-read-only (get-credit-score (user principal))
    (default-to {
        score: u500,
        loans-taken: u0,
        loans-repaid: u0,
    }
        (map-get? credit-scores { user: user })
    )
)
(define-constant approval-threshold u1000000)
(define-constant min-validators u3)
(define-constant err-insufficient-approvals (err u107))
(define-constant err-not-validator (err u108))
(define-constant err-already-voted (err u109))

(define-data-var validator-count uint u0)

(define-map validators
    { validator: principal }
    {
        active: bool,
        added-height: uint,
    }
)

(define-map loan-approvals
    { loan-id: uint }
    {
        required-approvals: uint,
        current-approvals: uint,
        approved: bool,
    }
)

(define-map validator-votes
    {
        loan-id: uint,
        validator: principal,
    }
    {
        voted: bool,
        vote-height: uint,
    }
)

(define-public (add-validator (validator principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set validators { validator: validator } {
            active: true,
            added-height: stacks-block-height,
        })
        (var-set validator-count (+ (var-get validator-count) u1))
        (ok true)
    )
)

(define-public (remove-validator (validator principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-some (map-get? validators { validator: validator }))
            err-not-found
        )
        (map-set validators { validator: validator } {
            active: false,
            added-height: stacks-block-height,
        })
        (var-set validator-count (- (var-get validator-count) u1))
        (ok true)
    )
)

(define-public (approve-loan (loan-id uint))
    (let (
            (loan (unwrap! (map-get? loans { loan-id: loan-id }) err-not-found))
            (validator-info (unwrap! (map-get? validators { validator: tx-sender })
                err-not-validator
            ))
            (approval-info (unwrap! (map-get? loan-approvals { loan-id: loan-id }) err-not-found))
            (existing-vote (map-get? validator-votes {
                loan-id: loan-id,
                validator: tx-sender,
            }))
        )
        (asserts! (get active validator-info) err-not-validator)
        (asserts! (is-none existing-vote) err-already-voted)
        (asserts! (is-eq (get status loan) "PENDING") err-loan-not-active)
        (map-set validator-votes {
            loan-id: loan-id,
            validator: tx-sender,
        } {
            voted: true,
            vote-height: stacks-block-height,
        })
        (let ((new-approvals (+ (get current-approvals approval-info) u1)))
            (map-set loan-approvals { loan-id: loan-id } {
                required-approvals: (get required-approvals approval-info),
                current-approvals: new-approvals,
                approved: (>= new-approvals (get required-approvals approval-info)),
            })
        )
        (ok true)
    )
)

(define-public (create-loan-with-approval
        (amount uint)
        (collateral uint)
        (interest-rate uint)
        (duration uint)
    )
    (let (
            (loan-id (+ (var-get next-loan-id) u1))
            (collateral-ratio (/ (* collateral u100) amount))
            (requires-approval (>= amount approval-threshold))
            (required-approvals (if requires-approval
                min-validators
                u0
            ))
        )
        (asserts! (>= collateral-ratio (var-get min-collateral-ratio))
            err-insufficient-collateral
        )
        (try! (stx-transfer? collateral tx-sender (as-contract tx-sender)))
        (map-set loans { loan-id: loan-id } {
            borrower: tx-sender,
            lender: none,
            amount: amount,
            collateral: collateral,
            interest-rate: interest-rate,
            duration: duration,
            status: (if requires-approval
                "PENDING_APPROVAL"
                "PENDING"
            ),
            start-height: u0,
            repaid-amount: u0,
        })
        (if requires-approval
            (map-set loan-approvals { loan-id: loan-id } {
                required-approvals: required-approvals,
                current-approvals: u0,
                approved: false,
            })
            true
        )
        (var-set next-loan-id loan-id)
        (ok loan-id)
    )
)

(define-read-only (get-approval-status (loan-id uint))
    (map-get? loan-approvals { loan-id: loan-id })
)

(define-read-only (is-validator (validator principal))
    (match (map-get? validators { validator: validator })
        validator-info (get active validator-info)
        false
    )
)
(define-constant insurance-fee-rate u50)
(define-constant max-coverage-ratio u80)
(define-constant reward-distribution-period u1008)
(define-constant err-insufficient-pool (err u110))
(define-constant err-invalid-claim (err u111))

(define-data-var total-pool-balance uint u0)
(define-data-var total-pool-shares uint u0)
(define-data-var last-reward-distribution uint u0)

(define-map pool-contributors
    { contributor: principal }
    {
        shares: uint,
        contribution-amount: uint,
        join-height: uint,
        rewards-claimed: uint,
    }
)

(define-map insurance-claims
    { loan-id: uint }
    {
        claim-amount: uint,
        claimed: bool,
        claim-height: uint,
    }
)

(define-map loan-insurance
    { loan-id: uint }
    {
        insured: bool,
        premium-paid: uint,
        coverage-amount: uint,
    }
)

(define-public (contribute-to-pool (amount uint))
    (let (
            (current-contributor (default-to {
                shares: u0,
                contribution-amount: u0,
                join-height: stacks-block-height,
                rewards-claimed: u0,
            }
                (map-get? pool-contributors { contributor: tx-sender })
            ))
            (current-pool-balance (var-get total-pool-balance))
            (current-total-shares (var-get total-pool-shares))
            (new-shares (if (is-eq current-pool-balance u0)
                amount
                (/ (* amount current-total-shares) current-pool-balance)
            ))
        )
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set pool-contributors { contributor: tx-sender } {
            shares: (+ (get shares current-contributor) new-shares),
            contribution-amount: (+ (get contribution-amount current-contributor) amount),
            join-height: (get join-height current-contributor),
            rewards-claimed: (get rewards-claimed current-contributor),
        })
        (var-set total-pool-balance (+ current-pool-balance amount))
        (var-set total-pool-shares (+ current-total-shares new-shares))
        (ok new-shares)
    )
)

(define-public (purchase-loan-insurance (loan-id uint))
    (let (
            (loan (unwrap! (map-get? loans { loan-id: loan-id }) err-not-found))
            (premium (/ (* (get amount loan) insurance-fee-rate) u10000))
            (coverage-amount (/ (* (get amount loan) max-coverage-ratio) u100))
        )
        (asserts! (is-eq (get borrower loan) tx-sender) err-unauthorized)
        (asserts! (is-eq (get status loan) "PENDING") err-loan-not-active)
        (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))
        (map-set loan-insurance { loan-id: loan-id } {
            insured: true,
            premium-paid: premium,
            coverage-amount: coverage-amount,
        })
        (var-set total-pool-balance (+ (var-get total-pool-balance) premium))
        (ok true)
    )
)

(define-public (claim-insurance (loan-id uint))
    (let (
            (loan (unwrap! (map-get? loans { loan-id: loan-id }) err-not-found))
            (insurance-info (unwrap! (map-get? loan-insurance { loan-id: loan-id }) err-not-found))
            (existing-claim (map-get? insurance-claims { loan-id: loan-id }))
            (coverage-amount (get coverage-amount insurance-info))
            (current-pool-balance (var-get total-pool-balance))
        )
        (asserts! (is-some (get lender loan)) err-not-found)
        (asserts! (is-eq tx-sender (unwrap! (get lender loan) err-not-found))
            err-unauthorized
        )
        (asserts!
            (or
                (is-eq (get status loan) "LIQUIDATED")
                (is-eq (get status loan) "DEFAULTED")
            )
            err-invalid-claim
        )
        (asserts! (get insured insurance-info) err-invalid-claim)
        (asserts! (is-none existing-claim) err-invalid-claim)
        (asserts! (>= current-pool-balance coverage-amount) err-insufficient-pool)
        (try! (as-contract (stx-transfer? coverage-amount tx-sender tx-sender)))
        (map-set insurance-claims { loan-id: loan-id } {
            claim-amount: coverage-amount,
            claimed: true,
            claim-height: stacks-block-height,
        })
        (var-set total-pool-balance (- current-pool-balance coverage-amount))
        (ok coverage-amount)
    )
)

(define-public (withdraw-from-pool (shares-to-withdraw uint))
    (let (
            (contributor-info (unwrap! (map-get? pool-contributors { contributor: tx-sender })
                err-not-found
            ))
            (user-shares (get shares contributor-info))
            (total-shares (var-get total-pool-shares))
            (total-balance (var-get total-pool-balance))
            (withdrawal-amount (/ (* shares-to-withdraw total-balance) total-shares))
        )
        (asserts! (<= shares-to-withdraw user-shares) err-invalid-amount)
        (asserts! (>= total-balance withdrawal-amount) err-insufficient-pool)
        (try! (as-contract (stx-transfer? withdrawal-amount tx-sender tx-sender)))
        (map-set pool-contributors { contributor: tx-sender } {
            shares: (- user-shares shares-to-withdraw),
            contribution-amount: (get contribution-amount contributor-info),
            join-height: (get join-height contributor-info),
            rewards-claimed: (get rewards-claimed contributor-info),
        })
        (var-set total-pool-balance (- total-balance withdrawal-amount))
        (var-set total-pool-shares (- total-shares shares-to-withdraw))
        (ok withdrawal-amount)
    )
)

(define-read-only (get-pool-stats)
    (ok {
        total-balance: (var-get total-pool-balance),
        total-shares: (var-get total-pool-shares),
        contributor-count: u0,
    })
)

(define-read-only (get-contributor-info (contributor principal))
    (map-get? pool-contributors { contributor: contributor })
)

(define-read-only (get-loan-insurance-info (loan-id uint))
    (map-get? loan-insurance { loan-id: loan-id })
)
(define-constant liquidation-threshold u120)
(define-constant liquidation-reward-rate u50)
(define-constant grace-period-blocks u1008)
(define-constant err-not-liquidatable (err u112))
(define-constant err-liquidation-failed (err u113))

(define-map liquidation-records
    { loan-id: uint }
    {
        liquidator: principal,
        liquidation-height: uint,
        collateral-seized: uint,
        liquidation-reward: uint,
    }
)

(define-public (liquidate-loan (loan-id uint))
    (let (
            (loan (unwrap! (map-get? loans { loan-id: loan-id }) err-not-found))
            (loan-for-interest {
                borrower: (get borrower loan),
                amount: (get amount loan),
                interest-rate: (get interest-rate loan),
                duration: (get duration loan),
                start-height: (get start-height loan),
            })
            (total-debt (+ (get amount loan) (calculate-interest loan-for-interest)))
            (repaid-amount (get repaid-amount loan))
            (remaining-debt (- total-debt repaid-amount))
            (collateral-value (get collateral loan))
            (current-ratio (/ (* collateral-value u100) remaining-debt))
            (blocks-since-start (- stacks-block-height (get start-height loan)))
            (is-overdue (> blocks-since-start (+ (get duration loan) grace-period-blocks)))
            (liquidation-reward (/ (* collateral-value liquidation-reward-rate) u10000))
            (lender-amount (- collateral-value liquidation-reward))
        )
        (asserts! (is-eq (get status loan) "ACTIVE") err-loan-not-active)
        (asserts!
            (or
                (< current-ratio liquidation-threshold)
                is-overdue
            )
            err-not-liquidatable
        )
        (asserts! (is-some (get lender loan)) err-not-found)
        (try! (as-contract (stx-transfer? liquidation-reward tx-sender tx-sender)))
        (try! (as-contract (stx-transfer? lender-amount tx-sender
            (unwrap! (get lender loan) err-not-found)
        )))
        (map-set loans { loan-id: loan-id } (merge loan { status: "LIQUIDATED" }))
        (map-set liquidation-records { loan-id: loan-id } {
            liquidator: tx-sender,
            liquidation-height: stacks-block-height,
            collateral-seized: collateral-value,
            liquidation-reward: liquidation-reward,
        })
        (ok {
            liquidation-reward: liquidation-reward,
            lender-recovery: lender-amount,
        })
    )
)

(define-read-only (check-liquidation-eligibility (loan-id uint))
    (match (map-get? loans { loan-id: loan-id })
        loan (let (
                (loan-for-interest {
                    borrower: (get borrower loan),
                    amount: (get amount loan),
                    interest-rate: (get interest-rate loan),
                    duration: (get duration loan),
                    start-height: (get start-height loan),
                })
                (total-debt (+ (get amount loan) (calculate-interest loan-for-interest)))
                (remaining-debt (- total-debt (get repaid-amount loan)))
                (current-ratio (/ (* (get collateral loan) u100) remaining-debt))
                (blocks-since-start (- stacks-block-height (get start-height loan)))
                (is-overdue (> blocks-since-start (+ (get duration loan) grace-period-blocks)))
            )
            (ok {
                liquidatable: (and
                    (is-eq (get status loan) "ACTIVE")
                    (or
                        (< current-ratio liquidation-threshold)
                        is-overdue
                    )
                ),
                current-ratio: current-ratio,
                is-overdue: is-overdue,
                remaining-debt: remaining-debt,
            })
        )
        (err err-not-found)
    )
)

(define-read-only (get-liquidation-record (loan-id uint))
    (map-get? liquidation-records { loan-id: loan-id })
)
(define-constant max-rate-change u200)
(define-constant min-base-rate u100)
(define-constant max-base-rate u2000)
(define-constant rate-update-cooldown u144)
(define-constant min-oracle-count u2)
(define-constant err-rate-out-of-bounds (err u114))
(define-constant err-oracle-cooldown (err u115))
(define-constant err-insufficient-oracles (err u116))

(define-data-var base-interest-rate uint u500)
(define-data-var last-rate-update uint u0)
(define-data-var oracle-count uint u0)

(define-map interest-rate-oracles
    { oracle: principal }
    {
        active: bool,
        weight: uint,
        last-update: uint,
        current-rate: uint,
    }
)

(define-map rate-proposals
    { proposal-id: uint }
    {
        proposed-rate: uint,
        proposer: principal,
        votes: uint,
        executed: bool,
        proposal-height: uint,
    }
)

(define-data-var next-proposal-id uint u0)

(define-public (add-oracle
        (oracle principal)
        (weight uint)
    )
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set interest-rate-oracles { oracle: oracle } {
            active: true,
            weight: weight,
            last-update: u0,
            current-rate: (var-get base-interest-rate),
        })
        (var-set oracle-count (+ (var-get oracle-count) u1))
        (ok true)
    )
)

(define-public (remove-oracle (oracle principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-some (map-get? interest-rate-oracles { oracle: oracle }))
            err-not-found
        )
        (map-set interest-rate-oracles { oracle: oracle } {
            active: false,
            weight: u0,
            last-update: stacks-block-height,
            current-rate: u0,
        })
        (var-set oracle-count (- (var-get oracle-count) u1))
        (ok true)
    )
)

(define-public (update-rate (new-rate uint))
    (let (
            (oracle-info (unwrap! (map-get? interest-rate-oracles { oracle: tx-sender })
                err-not-validator
            ))
            (current-base-rate (var-get base-interest-rate))
            (rate-change (if (> new-rate current-base-rate)
                (- new-rate current-base-rate)
                (- current-base-rate new-rate)
            ))
            (blocks-since-update (- stacks-block-height (get last-update oracle-info)))
        )
        (asserts! (get active oracle-info) err-not-validator)
        (asserts! (>= blocks-since-update rate-update-cooldown)
            err-oracle-cooldown
        )
        (asserts! (and (>= new-rate min-base-rate) (<= new-rate max-base-rate))
            err-rate-out-of-bounds
        )
        (asserts! (<= rate-change max-rate-change) err-rate-out-of-bounds)
        (map-set interest-rate-oracles { oracle: tx-sender }
            (merge oracle-info {
                last-update: stacks-block-height,
                current-rate: new-rate,
            })
        )
        (let ((new-weighted-rate (calculate-weighted-average-rate)))
            (var-set base-interest-rate new-weighted-rate)
            (var-set last-rate-update stacks-block-height)
            (ok new-weighted-rate)
        )
    )
)

(define-private (calculate-weighted-average-rate)
    (let (
            (oracle-1 (map-get? interest-rate-oracles { oracle: contract-owner }))
            (oracle-2 (map-get? interest-rate-oracles { oracle: contract-owner }))
        )
        (var-get base-interest-rate)
    )
)

(define-public (get-current-rate-for-loan
        (amount uint)
        (duration uint)
        (borrower principal)
    )
    (let (
            (base-rate (var-get base-interest-rate))
            (credit-info (get-credit-score borrower))
            (credit-adjustment (if (> (get score credit-info) u700)
                u50
                (if (< (get score credit-info) u400)
                    u100
                    u0
                )
            ))
            (duration-adjustment (if (> duration u4032)
                u25
                u0
            ))
            (amount-adjustment (if (> amount u10000000)
                u25
                u0
            ))
            (final-rate (+ base-rate credit-adjustment duration-adjustment amount-adjustment))
        )
        (ok final-rate)
    )
)

(define-public (create-loan-with-dynamic-rate
        (amount uint)
        (collateral uint)
        (duration uint)
    )
    (let (
            (dynamic-rate (unwrap! (get-current-rate-for-loan amount duration tx-sender)
                err-invalid-amount
            ))
            (loan-id (+ (var-get next-loan-id) u1))
            (collateral-ratio (/ (* collateral u100) amount))
            (requires-approval (>= amount approval-threshold))
            (required-approvals (if requires-approval
                min-validators
                u0
            ))
        )
        (asserts! (>= collateral-ratio (var-get min-collateral-ratio))
            err-insufficient-collateral
        )
        (try! (stx-transfer? collateral tx-sender (as-contract tx-sender)))
        (map-set loans { loan-id: loan-id } {
            borrower: tx-sender,
            lender: none,
            amount: amount,
            collateral: collateral,
            interest-rate: dynamic-rate,
            duration: duration,
            status: (if requires-approval
                "PENDING_APPROVAL"
                "PENDING"
            ),
            start-height: u0,
            repaid-amount: u0,
        })
        (if requires-approval
            (map-set loan-approvals { loan-id: loan-id } {
                required-approvals: required-approvals,
                current-approvals: u0,
                approved: false,
            })
            true
        )
        (var-set next-loan-id loan-id)
        (ok {
            loan-id: loan-id,
            interest-rate: dynamic-rate,
        })
    )
)

(define-read-only (get-oracle-info (oracle principal))
    (map-get? interest-rate-oracles { oracle: oracle })
)

(define-read-only (get-current-base-rate)
    (ok {
        base-rate: (var-get base-interest-rate),
        last-update: (var-get last-rate-update),
        oracle-count: (var-get oracle-count),
    })
)

(define-read-only (is-oracle (oracle principal))
    (match (map-get? interest-rate-oracles { oracle: oracle })
        oracle-info (get active oracle-info)
        false
    )
)
