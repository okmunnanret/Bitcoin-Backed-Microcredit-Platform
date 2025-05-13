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
