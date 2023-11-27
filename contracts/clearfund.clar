(define-constant CONTRACT_ADDRESS (as-contract tx-sender))
(define-constant DEPLOYER tx-sender)

(define-constant ERR_TITLE_DESCRIPTION_LINK_EMPTY (err u101))
(define-constant ERR_INVALID_FUND_GOAL (err u102))
(define-constant ERR_START_NOT_VALID (err u103))
(define-constant ERR_END_NOT_VALID (err u104))
(define-constant ERR_ID_NOT_FOUND (err u105))
(define-constant ERR_CANNOT_CANCEL (err u106))
(define-constant ERR_NOT_OWNER (err u107))
(define-constant ERR_NOT_STARTED (err u108))
(define-constant ERR_ENDED (err u109))
(define-constant ERR_PLEDGE_GREATER_THAN_ZERO (err u110))
(define-constant ERR_STX_TRANSFER_FAILED (err u111))
(define-constant ERR_NOT_PLEDGED (err u112))
(define-constant ERR_INVALID_UNPLEDGE_AMT (err u113))
(define-constant ERR_NOT_ENDED (err u114))
(define-constant ERR_GOAL_NOT_MET (err u115))
(define-constant ERR_ALREADY_CLAIMED (err u116))
(define-constant ERR_TARGET_NOT_REACHED (err u117))
(define-constant ERR_NOT_ENOUGH_BALANCE (err u118))

;; calculate roughly 90 days based on block times of 10 minutes
(define-constant FUNDING_TIME_LIMIT u12960)

(define-data-var last-id uint u0)

(define-map Campaigns uint {
    title: (string-utf8 256),
    description: (buff 33),
    link: (string-utf8 256),
    fundGoal: uint,
    startsAt: uint,
    endsAt: uint,
    campaignOwner: principal,
    pledgedCount: uint,
    pledgedAmount: uint,
    claimed: bool,
    targetReached: bool,
    targetReachedBy: uint
})

(define-map Investments {contributor: principal, campaignId: uint} {amount: uint})

;; -----------------
;; Public functions
;; -----------------

;; launch
;; Called by a user who wants to launch a new campaign
;; 
(define-public (launch (title (string-utf8 256)) 
                       (desp (buff 33)) 
                       (link (string-utf8 256))
                       (goal uint)
                       (begin-block uint)
                       (end-at uint)) 
    (let 
        (
            (current-id (var-get last-id))
            (next-id (+ current-id u1))
        )   

        (asserts! (> goal u0) ERR_INVALID_FUND_GOAL)
        (asserts! (>= begin-block block-height) ERR_START_NOT_VALID)
        (asserts! (>= end-at block-height) ERR_END_NOT_VALID)
        (asserts! (>= FUNDING_TIME_LIMIT (- end-at begin-block)) ERR_END_NOT_VALID)
        (asserts! 
            (not 
                (or (is-eq title u"") (is-eq link u"") (is-eq desp 0x0000))
            )
            ERR_TITLE_DESCRIPTION_LINK_EMPTY 
        )

        (map-set Campaigns current-id {
            title: title,
            description: desp, 
            link: link,
            fundGoal: goal,
            startsAt: begin-block,
            endsAt: end-at, 
            campaignOwner: tx-sender,
            pledgedCount: u0,
            pledgedAmount: u0,
            claimed: false,
            targetReached: false,
            targetReachedBy: u0 })

        (var-set last-id next-id)
        (ok next-id)
    )
)

;; get-campaign
(define-read-only (get-campaign (campaign-id uint))
    (ok 
        (unwrap! 
            (map-get? Campaigns campaign-id) 
            ERR_ID_NOT_FOUND
        )
    )
)

;; cancel 
;; Cancel a campaign 
(define-public (cancel (campaign-id uint)) 
    (let 
        (
            (current-campaign (unwrap! (get-campaign campaign-id) ERR_ID_NOT_FOUND))
            (started-block (get startsAt current-campaign))
            (campaign-owner (get campaignOwner current-campaign))
        ) 
        ;; can't cancel after it started
        (asserts! (< block-height started-block) ERR_CANNOT_CANCEL)
        (asserts! (is-eq campaign-owner tx-sender) ERR_NOT_OWNER)

        (ok (map-delete Campaigns campaign-id))
    )
)

;; update 
;; update the title, description and link 
;; Tx.contractCall('clearfund', 'update', [types.uint(1), types.utf8("New Title"), types.buff("New description"), types.utf8("https://newexample.org")], wallet_1)
 (define-public (update (campaign-id uint)
                        (title (string-utf8 256))
                        (desp (buff 33))
                        (link (string-utf8 256)))
    (let 
        (
            (found-campaign (unwrap! (get-campaign campaign-id) ERR_ID_NOT_FOUND ))
            (campaign-owner (get campaignOwner found-campaign))
            (end-block (get endsAt found-campaign))
        )

        (asserts! (is-eq tx-sender campaign-owner) ERR_NOT_OWNER)
        (asserts! (< block-height end-block) ERR_ENDED)

        (asserts! 
            (not 
                (or (is-eq title u"") (is-eq link u"") (is-eq desp 0x0000))
            )
            ERR_TITLE_DESCRIPTION_LINK_EMPTY 
        )

        (ok 
            (map-set Campaigns campaign-id 
                (merge 
                    found-campaign
                    {
                        title: title,
                        description: desp, 
                        link: link
                    }
                )
            )
        )
    )  
 )

(define-public (pledge (campaign-id uint) (amount uint))
    
    (let 
        (
            (found-campaign (unwrap! (get-campaign campaign-id) ERR_ID_NOT_FOUND ))
            (start-block (get startsAt found-campaign))
            (end-blcok (get endsAt found-campaign))
            (did-claimed (get claimed found-campaign))
        )

        ;; assert campaign is not finished 
        (asserts! (not (is-campaign-finished end-blcok)) ERR_ENDED)

        ;; assert campaign is started
        (asserts! (is-campaign-started start-block) ERR_NOT_STARTED)

        ;; assert campaign is not been claimed 
        (asserts! (is-eq did-claimed false) ERR_ALREADY_CLAIMED)

        ;; assert sender has enough balance 
        (asserts! ( > (stx-get-balance tx-sender) amount) ERR_NOT_ENOUGH_BALANCE)

        ;; stx-transfer to contract 
        (try! (stx-transfer? amount tx-sender CONTRACT_ADDRESS))

        ;; mint NFT for tx-sender
        (try! (contract-call? .donorpass mint tx-sender))

        ;; update campaign map
        (try! (update-campaign-after-pledged tx-sender campaign-id amount))

        ;; update investment map
        (ok (update-investment-after-pledged tx-sender campaign-id amount))
    )
)

(define-public (claim (campaign-id uint)) 

    (let  
        (
            (found-campaign (unwrap! (get-campaign campaign-id) ERR_ID_NOT_FOUND ))
            (campaign-owner (get campaignOwner found-campaign))
            (current-pledged (get pledgedAmount found-campaign))
            (target-reached (get targetReached found-campaign))
            (already-claimed (get claimed found-campaign))
        )

        ;; assert sender is the campaign owner 
        (asserts! (is-eq tx-sender campaign-owner) ERR_NOT_OWNER)
        (asserts! target-reached ERR_GOAL_NOT_MET)
        (asserts! (not already-claimed) ERR_ALREADY_CLAIMED)

        (asserts! (> current-pledged u0) ERR_NOT_ENOUGH_BALANCE)

        ;; stx transfer to campaign owner 
        (try! (as-contract (stx-transfer? current-pledged CONTRACT_ADDRESS campaign-owner)))

        ;; update map record for the campaign 

        (ok 
            (map-set Campaigns campaign-id 
                (merge 
                    found-campaign
                    {
                        claimed: true
                    }
                )
            )
        )
    )
)

(define-private (is-campaign-finished (end-at-block uint )) 

    (if 
        ( < end-at-block block-height) 
        true 
        false
    )
)

(define-private (is-campaign-started (start-at-block uint))
    (if 
        ( < start-at-block block-height)
        true 
        false
    )
)

(define-private (update-campaign-after-pledged (investor principal) (campaign-id uint) (pledge-amount uint)) 
    (let 
        (
            (found-campaign (unwrap! (get-campaign campaign-id) ERR_ID_NOT_FOUND ))
            (current-pledged-count (get pledgedCount found-campaign))
            (current-pledged-amount (get pledgedAmount found-campaign))
            (current-goal (get fundGoal found-campaign))
            (new-pledged-amount (+ current-pledged-amount pledge-amount))

            (already-invest-amount (get-investment-amount campaign-id tx-sender))
            (new-pledged-count (calculate-new-pledge-count already-invest-amount current-pledged-count))
        )

        (ok 
            (map-set Campaigns campaign-id 
                (if (>= new-pledged-amount current-goal)
                    ;; funding goal reached
                    (merge 
                        found-campaign
                        {
                            pledgedCount:  new-pledged-count, 
                            pledgedAmount: new-pledged-amount,
                            targetReached: true,
                            targetReachedBy: block-height
                        } 
                    )
                    
                    ;; funding goal not reached
                    (merge 
                        found-campaign
                        {
                            pledgedCount: new-pledged-count, 
                            pledgedAmount: new-pledged-amount
                        } 
                        
                    )
                )
            )
        )
    )
)

(define-private (update-investment-after-pledged (investor principal) (campaign-id uint) (pledge-amount uint)) 
    (let  
        (
            (existing-amount (get-investment-amount campaign-id investor))
        ) 
        (map-set Investments 
                {contributor: investor, campaignId: campaign-id} 
                {amount: (+ existing-amount pledge-amount)}
        )
    )
)

(define-private (calculate-new-pledge-count (existing-invest-amount uint) (current-pledged-count uint)) 
    (if (and (> existing-invest-amount u0) (> current-pledged-count u0))
        current-pledged-count
        (+ current-pledged-count u1) 
    )           
)

(define-read-only (get-investment-amount (campaign-id uint) (investor principal) ) 
    (let 
        (
            (investment-key {contributor: investor, campaignId: campaign-id})
            (existing-invest-amount (map-get? Investments investment-key))
        )
        (default-to u0 (get amount existing-invest-amount))
    )
)

(define-read-only (get-investment (campaign-id uint) (investor principal) ) 
    (let 
        (
            (investment-key {contributor: investor, campaignId: campaign-id})
            (existing-invest-amount (map-get? Investments investment-key))
        )
        (ok existing-invest-amount)
    )
)
