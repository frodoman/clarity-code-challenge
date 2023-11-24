
(impl-trait 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait.nft-trait)

(define-constant DEPLOYER  tx-sender)
(define-constant CLEARFUND_CONTRACT .clearfund)
(define-constant ERR_CLEARFUND_ONLY (err u100))
(define-constant ERR_NOT_TOKEN_OWNER (err u101))
(define-constant ERR_MISSING_TOKEN_OWNER (err u102))

(define-non-fungible-token donorpass uint)

(define-data-var lastTokenId uint u0)

;; 
;; Trait functions
;; 

;; (get-last-token-id () (response uint uint))
(define-public (get-last-token-id)  
    (ok (var-get lastTokenId))
)

;; (get-owner (uint) (response (optional principal) uint))
(define-public (get-owner (nft-id uint))
    (ok (nft-get-owner? donorpass nft-id))
)

;; (get-token-uri (uint) (response (optional (string-ascii 256)) uint))
(define-public (get-token-uri (nft-id uint))
     (ok none)
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
	(begin
		(asserts! (is-eq tx-sender sender) ERR_NOT_TOKEN_OWNER)
		(nft-transfer? donorpass token-id sender recipient)
	)
)

(define-public (mint (recipient principal)) 
    (let 
        (
            (current-id (var-get lastTokenId))
            (next-id (+ current-id u1))
        )

        (asserts! (is-eq contract-caller CLEARFUND_CONTRACT) ERR_CLEARFUND_ONLY)
        (try! (nft-mint? donorpass next-id recipient))

        (var-set lastTokenId next-id)

        (ok tx-sender)
    )
)