(impl-trait 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait.nft-trait)

(define-constant CLEARFUND_CONTRACT .clearfund)
(define-constant ERR_CLEARFUND_ONLY (err u100))
(define-constant ERR_NOT_TOKEN_OWNER (err u101))
(define-constant ERR_MISSING_TOKEN_OWNER (err u102))

(define-non-fungible-token donorpass uint)

(define-data-var lastTokenId uint u0)
(define-map nft-urls uint (string-ascii 256))

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
     (ok (map-get? nft-urls nft-id))
)

;; (transfer (uint principal principal) (response bool uint))
(define-public (transfer (nft-id uint) (from  principal) (to principal)) 
    (let  
        (
            (nft-owner (unwrap! (get-owner nft-id) ERR_MISSING_TOKEN_OWNER))
        )

        ;;(asserts! (is-eq nft-owner (some from)) ERR_NOT_TOKEN_OWNER)
        (nft-transfer? donorpass nft-id from to)
    )
   
)

