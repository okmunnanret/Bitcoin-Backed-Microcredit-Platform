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

(define-constant min-refinance-improvement u100)
(define-constant refinance-fee-rate u25)
(define-constant max-refinance-count u5)
(define-constant err-no-improvement (err u117))
(define-constant err-max-refinances (err u118))
(define-constant err-refinance-failed (err u119))

(define-data-var next-refinance-id uint u0)

(define-map loan-refinance-history
    { original-loan-id: uint }
    {
        refinance-count: uint,
        last-refinance-height: uint,
        original-rate: uint,
        current-rate: uint,
    }
)

(define-map refinance-records
    { refinance-id: uint }
    {
        original-loan-id: uint,
        new-loan-id: uint,
        borrower: principal,
        old-rate: uint,
        new-rate: uint,
        old-amount: uint,
        new-amount: uint,
        refinance-height: uint,
        fee-paid: uint,
    }
)

(define-public (refinance-loan
        (loan-id uint)
        (new-collateral uint)
    )
    (let (
            (loan (unwrap! (map-get? loans { loan-id: loan-id }) err-not-found))
            (current-rate (get interest-rate loan))
            (remaining-amount (- (get amount loan) (get repaid-amount loan)))
            (new-rate (unwrap!
                (get-current-rate-for-loan remaining-amount (get duration loan)
                    tx-sender
                )
                err-invalid-amount
            ))
            (rate-improvement (- current-rate new-rate))
            (refinance-history (default-to {
                refinance-count: u0,
                last-refinance-height: u0,
                original-rate: current-rate,
                current-rate: current-rate,
            }
                (map-get? loan-refinance-history { original-loan-id: loan-id })
            ))
            (refinance-fee (/ (* remaining-amount refinance-fee-rate) u10000))
            (new-loan-id (+ (var-get next-loan-id) u1))
            (refinance-id (+ (var-get next-refinance-id) u1))
            (total-collateral (+ (get collateral loan) new-collateral))
            (collateral-ratio (/ (* total-collateral u100) remaining-amount))
        )
        (asserts! (is-eq (get borrower loan) tx-sender) err-unauthorized)
        (asserts! (is-eq (get status loan) "ACTIVE") err-loan-not-active)
        (asserts! (>= rate-improvement min-refinance-improvement)
            err-no-improvement
        )
        (asserts! (< (get refinance-count refinance-history) max-refinance-count)
            err-max-refinances
        )
        (asserts! (>= collateral-ratio (var-get min-collateral-ratio))
            err-insufficient-collateral
        )
        (if (> new-collateral u0)
            (try! (stx-transfer? new-collateral tx-sender (as-contract tx-sender)))
            true
        )
        (try! (stx-transfer? refinance-fee tx-sender (as-contract tx-sender)))
        (map-set loans { loan-id: loan-id } (merge loan { status: "REFINANCED" }))
        (map-set loans { loan-id: new-loan-id } {
            borrower: tx-sender,
            lender: (get lender loan),
            amount: remaining-amount,
            collateral: total-collateral,
            interest-rate: new-rate,
            duration: (get duration loan),
            status: "ACTIVE",
            start-height: stacks-block-height,
            repaid-amount: u0,
        })
        (map-set loan-refinance-history { original-loan-id: loan-id } {
            refinance-count: (+ (get refinance-count refinance-history) u1),
            last-refinance-height: stacks-block-height,
            original-rate: (get original-rate refinance-history),
            current-rate: new-rate,
        })
        (map-set refinance-records { refinance-id: refinance-id } {
            original-loan-id: loan-id,
            new-loan-id: new-loan-id,
            borrower: tx-sender,
            old-rate: current-rate,
            new-rate: new-rate,
            old-amount: (get amount loan),
            new-amount: remaining-amount,
            refinance-height: stacks-block-height,
            fee-paid: refinance-fee,
        })
        (var-set next-loan-id new-loan-id)
        (var-set next-refinance-id refinance-id)
        (ok {
            new-loan-id: new-loan-id,
            old-rate: current-rate,
            new-rate: new-rate,
            rate-savings: rate-improvement,
            fee-paid: refinance-fee,
        })
    )
)

(define-read-only (get-refinance-eligibility (loan-id uint))
    (match (map-get? loans { loan-id: loan-id })
        loan (let (
                (current-rate (get interest-rate loan))
                (remaining-amount (- (get amount loan) (get repaid-amount loan)))
                (new-rate-result (get-current-rate-for-loan remaining-amount (get duration loan)
                    (get borrower loan)
                ))
                (refinance-history (default-to {
                    refinance-count: u0,
                    last-refinance-height: u0,
                    original-rate: current-rate,
                    current-rate: current-rate,
                }
                    (map-get? loan-refinance-history { original-loan-id: loan-id })
                ))
            )
            (if (is-ok new-rate-result)
                (let (
                        (new-rate (unwrap-panic new-rate-result))
                        (rate-improvement (if (> current-rate new-rate)
                            (- current-rate new-rate)
                            u0
                        ))
                        (is-eligible (and
                            (is-eq (get status loan) "ACTIVE")
                            (>= rate-improvement min-refinance-improvement)
                            (< (get refinance-count refinance-history)
                                max-refinance-count
                            )
                        ))
                    )
                    (ok {
                        eligible: is-eligible,
                        current-rate: current-rate,
                        new-rate: new-rate,
                        rate-savings: rate-improvement,
                        refinance-count: (get refinance-count refinance-history),
                        remaining-amount: remaining-amount,
                    })
                )
                (err err-invalid-amount)
            )
        )
        (err err-not-found)
    )
)

(define-read-only (get-refinance-record (refinance-id uint))
    (map-get? refinance-records { refinance-id: refinance-id })
)

(define-read-only (get-loan-refinance-history (loan-id uint))
    (map-get? loan-refinance-history { original-loan-id: loan-id })
)

(define-constant health-warning-threshold u140)
(define-constant health-critical-threshold u125)
(define-constant payment-warning-blocks u720)
(define-constant health-check-reward u100)
(define-constant err-loan-healthy (err u120))
(define-constant err-alert-exists (err u121))

(define-data-var next-alert-id uint u0)

(define-map loan-health-alerts
    { loan-id: uint }
    {
        health-score: uint,
        alert-level: (string-ascii 20),
        last-check-height: uint,
        payment-due-blocks: uint,
        triggered-by: principal,
    }
)

(define-map health-check-rewards
    { checker: principal }
    {
        total-rewards: uint,
        successful-checks: uint,
        last-reward-height: uint,
    }
)

(define-private (calculate-loan-health-score (loan-id uint))
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
                (payment-progress (/ (* (get repaid-amount loan) u100) total-debt))
                (time-progress (/ (* blocks-since-start u100) (get duration loan)))
                (payment-velocity-score (if (> time-progress u0)
                    (/ payment-progress time-progress)
                    u100
                ))
                (health-score (/ (+ current-ratio payment-velocity-score) u2))
            )
            (ok health-score)
        )
        (err err-not-found)
    )
)

(define-public (trigger-health-alert (loan-id uint))
    (let (
            (loan (unwrap! (map-get? loans { loan-id: loan-id }) err-not-found))
            (health-score (unwrap! (calculate-loan-health-score loan-id) err-not-found))
            (existing-alert (map-get? loan-health-alerts { loan-id: loan-id }))
            (blocks-since-start (- stacks-block-height (get start-height loan)))
            (payment-due-blocks (- (get duration loan) blocks-since-start))
            (alert-level (if (< health-score health-critical-threshold)
                "CRITICAL"
                (if (< health-score health-warning-threshold)
                    "WARNING"
                    "HEALTHY"
                )
            ))
            (reward-amount (if (or
                    (is-eq alert-level "WARNING")
                    (is-eq alert-level "CRITICAL")
                )
                health-check-reward
                u0
            ))
        )
        (asserts! (is-eq (get status loan) "ACTIVE") err-loan-not-active)
        (asserts! (not (is-eq alert-level "HEALTHY")) err-loan-healthy)
        (asserts! (is-none existing-alert) err-alert-exists)
        (map-set loan-health-alerts { loan-id: loan-id } {
            health-score: health-score,
            alert-level: alert-level,
            last-check-height: stacks-block-height,
            payment-due-blocks: payment-due-blocks,
            triggered-by: tx-sender,
        })
        (if (> reward-amount u0)
            (let ((checker-rewards (default-to {
                    total-rewards: u0,
                    successful-checks: u0,
                    last-reward-height: u0,
                }
                    (map-get? health-check-rewards { checker: tx-sender })
                )))
                (try! (as-contract (stx-transfer? reward-amount tx-sender tx-sender)))
                (map-set health-check-rewards { checker: tx-sender } {
                    total-rewards: (+ (get total-rewards checker-rewards) reward-amount),
                    successful-checks: (+ (get successful-checks checker-rewards) u1),
                    last-reward-height: stacks-block-height,
                })
                (ok {
                    alert-triggered: true,
                    health-score: health-score,
                    alert-level: alert-level,
                    reward-earned: reward-amount,
                })
            )
            (ok {
                alert-triggered: true,
                health-score: health-score,
                alert-level: alert-level,
                reward-earned: u0,
            })
        )
    )
)

(define-public (update-health-status (loan-id uint))
    (let (
            (loan (unwrap! (map-get? loans { loan-id: loan-id }) err-not-found))
            (health-score (unwrap! (calculate-loan-health-score loan-id) err-not-found))
            (existing-alert (map-get? loan-health-alerts { loan-id: loan-id }))
            (blocks-since-start (- stacks-block-height (get start-height loan)))
            (payment-due-blocks (- (get duration loan) blocks-since-start))
            (alert-level (if (< health-score health-critical-threshold)
                "CRITICAL"
                (if (< health-score health-warning-threshold)
                    "WARNING"
                    "HEALTHY"
                )
            ))
        )
        (asserts! (is-eq (get status loan) "ACTIVE") err-loan-not-active)
        (if (is-some existing-alert)
            (map-set loan-health-alerts { loan-id: loan-id } {
                health-score: health-score,
                alert-level: alert-level,
                last-check-height: stacks-block-height,
                payment-due-blocks: payment-due-blocks,
                triggered-by: (get triggered-by (unwrap-panic existing-alert)),
            })
            (if (not (is-eq alert-level "HEALTHY"))
                (map-set loan-health-alerts { loan-id: loan-id } {
                    health-score: health-score,
                    alert-level: alert-level,
                    last-check-height: stacks-block-height,
                    payment-due-blocks: payment-due-blocks,
                    triggered-by: tx-sender,
                })
                true
            )
        )
        (ok {
            health-score: health-score,
            alert-level: alert-level,
            payment-due-blocks: payment-due-blocks,
        })
    )
)

(define-public (clear-health-alert (loan-id uint))
    (let (
            (loan (unwrap! (map-get? loans { loan-id: loan-id }) err-not-found))
            (health-score (unwrap! (calculate-loan-health-score loan-id) err-not-found))
        )
        (asserts!
            (or
                (is-eq (get borrower loan) tx-sender)
                (is-eq tx-sender contract-owner)
            )
            err-unauthorized
        )
        (asserts! (>= health-score health-warning-threshold) err-loan-healthy)
        (map-delete loan-health-alerts { loan-id: loan-id })
        (ok true)
    )
)

(define-read-only (get-loan-health-status (loan-id uint))
    (match (map-get? loans { loan-id: loan-id })
        loan (let (
                (health-score-result (calculate-loan-health-score loan-id))
                (alert-info (map-get? loan-health-alerts { loan-id: loan-id }))
                (blocks-since-start (- stacks-block-height (get start-height loan)))
                (payment-due-blocks (- (get duration loan) blocks-since-start))
            )
            (match health-score-result
                health-score (ok {
                    loan-id: loan-id,
                    borrower: (get borrower loan),
                    health-score: health-score,
                    current-status: (get status loan),
                    payment-due-blocks: payment-due-blocks,
                    has-alert: (is-some alert-info),
                    alert-level: (if (is-some alert-info)
                        (get alert-level (unwrap-panic alert-info))
                        "HEALTHY"
                    ),
                    last-check: (if (is-some alert-info)
                        (get last-check-height (unwrap-panic alert-info))
                        u0
                    ),
                })
                error-value (err error-value)
            )
        )
        (err err-not-found)
    )
)

(define-read-only (get-health-checker-stats (checker principal))
    (map-get? health-check-rewards { checker: checker })
)

(define-read-only (get-loans-needing-health-check)
    (ok {
        warning-threshold: health-warning-threshold,
        critical-threshold: health-critical-threshold,
        payment-warning-blocks: payment-warning-blocks,
        check-reward: health-check-reward,
    })
)

;; ========================================
;; LOAN STATUS TRACKER FEATURE
;; ========================================

(define-constant max-events-per-loan u50)
(define-constant err-max-events-reached (err u122))
(define-constant err-invalid-event-type (err u123))

(define-data-var next-event-id uint u0)
(define-data-var total-tracked-loans uint u0)

;; Loan event types
(define-constant event-loan-created "LOAN_CREATED")
(define-constant event-loan-funded "LOAN_FUNDED")
(define-constant event-payment-made "PAYMENT_MADE")
(define-constant event-loan-completed "LOAN_COMPLETED")
(define-constant event-loan-liquidated "LOAN_LIQUIDATED")
(define-constant event-loan-refinanced "LOAN_REFINANCED")
(define-constant event-insurance-purchased "INSURANCE_PURCHASED")
(define-constant event-health-alert "HEALTH_ALERT")
(define-constant event-status-changed "STATUS_CHANGED")

;; Map to track loan events chronologically
(define-map loan-events
    { event-id: uint }
    {
        loan-id: uint,
        event-type: (string-ascii 20),
        event-data: (string-ascii 100),
        actor: principal,
        block-height: uint,
        timestamp-estimate: uint,
        additional-info: (optional (string-ascii 50)),
    }
)

;; Map to track events by loan ID for easy querying
(define-map loan-event-log
    { loan-id: uint }
    {
        event-count: uint,
        first-event-id: uint,
        last-event-id: uint,
        created-height: uint,
        last-activity-height: uint,
    }
)

;; Track loan performance metrics
(define-map loan-performance-metrics
    { loan-id: uint }
    {
        total-payments: uint,
        payment-count: uint,
        average-payment-size: uint,
        days-to-first-payment: uint,
        on-time-payments: uint,
        late-payments: uint,
        current-streak: uint,
        max-streak: uint,
    }
)

;; User activity summary
(define-map user-loan-activity
    { user: principal }
    {
        total-loans-created: uint,
        total-loans-funded: uint,
        active-borrower-loans: uint,
        active-lender-loans: uint,
        completed-loans: uint,
        defaulted-loans: uint,
        total-volume-borrowed: uint,
        total-volume-lent: uint,
    }
)

;; Private function to log loan events
(define-private (log-loan-event
        (loan-id uint)
        (event-type (string-ascii 20))
        (event-data (string-ascii 100))
        (actor principal)
        (additional-info (optional (string-ascii 50)))
    )
    (let (
            (event-id (+ (var-get next-event-id) u1))
            (current-log (default-to {
                event-count: u0,
                first-event-id: u0,
                last-event-id: u0,
                created-height: stacks-block-height,
                last-activity-height: stacks-block-height,
            }
                (map-get? loan-event-log { loan-id: loan-id })
            ))
            (timestamp-estimate (+ u1672531200 (* (- stacks-block-height u1) u600)))
        )
        (asserts! (< (get event-count current-log) max-events-per-loan)
            err-max-events-reached
        )
        (map-set loan-events { event-id: event-id } {
            loan-id: loan-id,
            event-type: event-type,
            event-data: event-data,
            actor: actor,
            block-height: stacks-block-height,
            timestamp-estimate: timestamp-estimate,
            additional-info: additional-info,
        })
        (map-set loan-event-log { loan-id: loan-id } {
            event-count: (+ (get event-count current-log) u1),
            first-event-id: (if (is-eq (get event-count current-log) u0)
                event-id
                (get first-event-id current-log)
            ),
            last-event-id: event-id,
            created-height: (get created-height current-log),
            last-activity-height: stacks-block-height,
        })
        (var-set next-event-id event-id)
        (ok event-id)
    )
)

;; Public function to manually log status changes (for admin use)
(define-public (log-status-change
        (loan-id uint)
        (old-status (string-ascii 20))
        (new-status (string-ascii 20))
        (reason (string-ascii 50))
    )
    (let (
            (loan (unwrap! (map-get? loans { loan-id: loan-id }) err-not-found))
            (event-data (concat "Status: " (concat old-status (concat " -> " new-status))))
        )
        (asserts!
            (or
                (is-eq tx-sender contract-owner)
                (is-eq tx-sender (get borrower loan))
                (match (get lender loan)
                    lender-principal (is-eq tx-sender lender-principal)
                    false
                )
            )
            err-unauthorized
        )
        (try! (log-loan-event loan-id event-status-changed event-data tx-sender (some reason)))
        (ok true)
    )
)

;; Update user activity metrics
(define-private (update-user-activity
        (user principal)
        (activity-type (string-ascii 20))
        (amount uint)
    )
    (let (
            (current-activity (default-to {
                total-loans-created: u0,
                total-loans-funded: u0,
                active-borrower-loans: u0,
                active-lender-loans: u0,
                completed-loans: u0,
                defaulted-loans: u0,
                total-volume-borrowed: u0,
                total-volume-lent: u0,
            }
                (map-get? user-loan-activity { user: user })
            ))
        )
        (if (is-eq activity-type "LOAN_CREATED")
            (map-set user-loan-activity { user: user }
                (merge current-activity {
                    total-loans-created: (+ (get total-loans-created current-activity) u1),
                    active-borrower-loans: (+ (get active-borrower-loans current-activity) u1),
                    total-volume-borrowed: (+ (get total-volume-borrowed current-activity) amount),
                })
            )
            (if (is-eq activity-type "LOAN_FUNDED")
                (map-set user-loan-activity { user: user }
                    (merge current-activity {
                        total-loans-funded: (+ (get total-loans-funded current-activity) u1),
                        active-lender-loans: (+ (get active-lender-loans current-activity) u1),
                        total-volume-lent: (+ (get total-volume-lent current-activity) amount),
                    })
                )
                (if (is-eq activity-type "LOAN_COMPLETED")
                    (map-set user-loan-activity { user: user }
                        (merge current-activity {
                            completed-loans: (+ (get completed-loans current-activity) u1),
                            active-borrower-loans: (if (> (get active-borrower-loans current-activity) u0)
                                (- (get active-borrower-loans current-activity) u1)
                                u0
                            ),
                        })
                    )
                    true
                )
            )
        )
        (ok true)
    )
)

;; Enhanced create-loan function with event logging
(define-public (create-loan-with-tracking
        (amount uint)
        (collateral uint)
        (interest-rate uint)
        (duration uint)
    )
    (let (
            (loan-result (create-loan-with-approval amount collateral interest-rate duration))
            (loan-id (unwrap! loan-result err-invalid-amount))
        )
        (try! (log-loan-event loan-id event-loan-created 
            (concat "Amount: " (uint-to-ascii amount))
            tx-sender
            (some (concat "Rate: " (uint-to-ascii interest-rate)))
        ))
        (unwrap! (update-user-activity tx-sender "LOAN_CREATED" amount) err-invalid-amount)
        (var-set total-tracked-loans (+ (var-get total-tracked-loans) u1))
        (ok loan-id)
    )
)

;; Enhanced fund-loan function with event logging
(define-public (fund-loan-with-tracking (loan-id uint))
    (let (
            (loan (unwrap! (map-get? loans { loan-id: loan-id }) err-not-found))
            (amount (get amount loan))
        )
        (try! (fund-loan loan-id))
        (try! (log-loan-event loan-id event-loan-funded
            (concat "Funded: " (uint-to-ascii amount))
            tx-sender
            (some "Loan activated")
        ))
        (unwrap! (update-user-activity tx-sender "LOAN_FUNDED" amount) err-invalid-amount)
        (ok true)
    )
)

;; Enhanced repay-loan function with event logging and performance tracking
(define-public (repay-loan-with-tracking
        (loan-id uint)
        (payment uint)
    )
    (let (
            (loan (unwrap! (map-get? loans { loan-id: loan-id }) err-not-found))
            (current-metrics (default-to {
                total-payments: u0,
                payment-count: u0,
                average-payment-size: u0,
                days-to-first-payment: u0,
                on-time-payments: u0,
                late-payments: u0,
                current-streak: u0,
                max-streak: u0,
            }
                (map-get? loan-performance-metrics { loan-id: loan-id })
            ))
            (new-payment-count (+ (get payment-count current-metrics) u1))
            (new-total-payments (+ (get total-payments current-metrics) payment))
            (new-average (/ new-total-payments new-payment-count))
            (blocks-since-start (- stacks-block-height (get start-height loan)))
            (is-first-payment (is-eq (get payment-count current-metrics) u0))
            (days-to-first (if is-first-payment (/ blocks-since-start u144) u0))
        )
        (try! (repay-loan loan-id payment))
        (try! (log-loan-event loan-id event-payment-made
            (concat "Payment: " (uint-to-ascii payment))
            tx-sender
            (some (concat "Remaining: " (uint-to-ascii (- (get amount loan) (get repaid-amount loan)))))
        ))
        (map-set loan-performance-metrics { loan-id: loan-id } {
            total-payments: new-total-payments,
            payment-count: new-payment-count,
            average-payment-size: new-average,
            days-to-first-payment: (if is-first-payment days-to-first (get days-to-first-payment current-metrics)),
            on-time-payments: (+ (get on-time-payments current-metrics) u1),
            late-payments: (get late-payments current-metrics),
            current-streak: (+ (get current-streak current-metrics) u1),
            max-streak: (if (> (+ (get current-streak current-metrics) u1) (get max-streak current-metrics))
                (+ (get current-streak current-metrics) u1)
                (get max-streak current-metrics)
            ),
        })
        ;; Check if loan is completed
        (let ((updated-loan (unwrap! (map-get? loans { loan-id: loan-id }) err-not-found)))
            (if (is-eq (get status updated-loan) "COMPLETED")
                (begin
                    (try! (log-loan-event loan-id event-loan-completed
                        "Loan fully repaid"
                        tx-sender
                        (some "Final payment")
                    ))
                    (unwrap! (update-user-activity tx-sender "LOAN_COMPLETED" u0) err-invalid-amount)
                    (ok true)
                )
                (ok true)
            )
        )
    )
)

;; Read-only functions to query loan tracking data

(define-read-only (get-loan-events (loan-id uint) (limit uint) (offset uint))
    (match (map-get? loan-event-log { loan-id: loan-id })
        log-info (let (
                (start-event-id (+ (get first-event-id log-info) offset))
                (target-end (+ start-event-id limit))
                (last-event (get last-event-id log-info))
                (end-event-id (if (< target-end last-event) target-end last-event))
            )
            (ok {
                loan-id: loan-id,
                total-events: (get event-count log-info),
                events-returned: (if (> end-event-id start-event-id) (- end-event-id start-event-id) u0),
                first-event-id: (get first-event-id log-info),
                last-event-id: (get last-event-id log-info),
            })
        )
        (err err-not-found)
    )
)

(define-read-only (get-loan-event-details (event-id uint))
    (ok (map-get? loan-events { event-id: event-id }))
)

(define-read-only (get-loan-performance-metrics (loan-id uint))
    (ok (map-get? loan-performance-metrics { loan-id: loan-id }))
)

(define-read-only (get-user-activity-summary (user principal))
    (ok (map-get? user-loan-activity { user: user }))
)

(define-read-only (get-platform-tracking-stats)
    (ok {
        total-tracked-loans: (var-get total-tracked-loans),
        total-events-logged: (var-get next-event-id),
        max-events-per-loan: max-events-per-loan,
    })
)

(define-read-only (get-loan-timeline-summary (loan-id uint))
    (match (map-get? loans { loan-id: loan-id })
        loan (match (map-get? loan-event-log { loan-id: loan-id })
            event-log (let (
                    (performance (map-get? loan-performance-metrics { loan-id: loan-id }))
                    (insurance-info (map-get? loan-insurance { loan-id: loan-id }))
                    (health-alert (map-get? loan-health-alerts { loan-id: loan-id }))
                )
                (ok {
                    loan-basic-info: {
                        loan-id: loan-id,
                        borrower: (get borrower loan),
                        lender: (get lender loan),
                        amount: (get amount loan),
                        status: (get status loan),
                        collateral: (get collateral loan),
                        interest-rate: (get interest-rate loan),
                    },
                    tracking-info: {
                        created-height: (get created-height event-log),
                        last-activity-height: (get last-activity-height event-log),
                        total-events: (get event-count event-log),
                        days-active: (/ (- (get last-activity-height event-log) (get created-height event-log)) u144),
                    },
                    performance-metrics: performance,
                    has-insurance: (is-some insurance-info),
                    has-health-alert: (is-some health-alert),
                })
            )
            (err err-not-found)
        )
        (err err-not-found)
    )
)

;; Utility function to convert uint to ascii (simplified version)
(define-private (uint-to-ascii (num uint))
    (if (is-eq num u0)
        "0"
        (if (<= num u9)
            (if (is-eq num u1) "1"
                (if (is-eq num u2) "2"
                    (if (is-eq num u3) "3"
                        (if (is-eq num u4) "4"
                            (if (is-eq num u5) "5"
                                (if (is-eq num u6) "6"
                                    (if (is-eq num u7) "7"
                                        (if (is-eq num u8) "8" "9")
                                    )
                                )
                            )
                        )
                    )
                )
            )
            "big-number"
        )
    )
)
