;; mintMart - Common Collectible NFT Contract (v2)
;; Rarity: Common | Supply: 10,000 | Price: 0.01 STX (10000 microSTX)

;; SIP-009 NFT Trait
(impl-trait 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait.nft-trait)

;; -------------------------------------------------
;; Constants
;; -------------------------------------------------

(define-constant ERR-NOT-OWNER (err u100))
(define-constant ERR-SOLD-OUT (err u101))
(define-constant ERR-WRONG-PRICE (err u102))
(define-constant ERR-NOT-TOKEN-OWNER (err u103))
(define-constant ERR-MINT-PAUSED (err u104))
(define-constant ERR-MAX-PER-WALLET (err u105))
(define-constant ERR-METADATA-FROZEN (err u106))

(define-constant MINT-PRICE u10000) ;; 0.01 STX
(define-constant MAX-SUPPLY u10000)
(define-constant MAX-PER-WALLET u5)
(define-constant ROYALTY-BPS u500) ;; 5%

;; -------------------------------------------------
;; Data Variables
;; -------------------------------------------------

(define-data-var contract-owner principal tx-sender)
(define-data-var last-token-id uint u0)
(define-data-var mint-paused bool false)
(define-data-var metadata-frozen bool false)
(define-data-var base-uri (string-ascii 200) "https://mintmart.io/api/metadata/common/")

;; Track per-wallet mint count
(define-map wallet-mints principal uint)

;; -------------------------------------------------
;; NFT Definition
;; -------------------------------------------------

(define-non-fungible-token common-collectible uint)

;; -------------------------------------------------
;; SIP-009 Required Read-Only
;; -------------------------------------------------

(define-read-only (get-last-token-id)
  (ok (var-get last-token-id)))

(define-read-only (get-owner (token-id uint))
  (ok (nft-get-owner? common-collectible token-id)))

(define-read-only (get-token-uri (token-id uint))
  (ok (some (concat (var-get base-uri) (to-string token-id)))))

;; -------------------------------------------------
;; Public Mint
;; -------------------------------------------------

(define-public (mint)
  (let (
        (next-id (+ (var-get last-token-id) u1))
        (minted (default-to u0 (map-get? wallet-mints tx-sender)))
       )
    
    (asserts! (not (var-get mint-paused)) ERR-MINT-PAUSED)
    (asserts! (<= next-id MAX-SUPPLY) ERR-SOLD-OUT)
    (asserts! (< minted MAX-PER-WALLET) ERR-MAX-PER-WALLET)

    ;; Transfer STX to contract
    (try! (stx-transfer? MINT-PRICE tx-sender (as-contract tx-sender)))

    ;; Mint NFT
    (try! (nft-mint? common-collectible next-id tx-sender))

    ;; Update state
    (var-set last-token-id next-id)
    (map-set wallet-mints tx-sender (+ minted u1))

    ;; Emit event
    (print {event: "mint", token-id: next-id, minter: tx-sender})

    (ok next-id)
  )
)

;; -------------------------------------------------
;; Batch Mint (max 10 per call)
;; -------------------------------------------------

(define-public (mint-many (count uint))
  (let (
        (start (var-get last-token-id))
        (minted (default-to u0 (map-get? wallet-mints tx-sender)))
       )

    (asserts! (not (var-get mint-paused)) ERR-MINT-PAUSED)
    (asserts! (> count u0) ERR-WRONG-PRICE)
    (asserts! (<= count u10) ERR-WRONG-PRICE)
    (asserts! (<= (+ start count) MAX-SUPPLY) ERR-SOLD-OUT)
    (asserts! (<= (+ minted count) MAX-PER-WALLET) ERR-MAX-PER-WALLET)

    ;; Transfer full payment
    (try! (stx-transfer? (* MINT-PRICE count) tx-sender (as-contract tx-sender)))

    (var-set last-token-id (+ start count))
    (map-set wallet-mints tx-sender (+ minted count))

    (print {event: "batch-mint", amount: count, minter: tx-sender})

    (ok true)
  )
)

;; -------------------------------------------------
;; Transfer (SIP-009)
;; -------------------------------------------------

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) ERR-NOT-TOKEN-OWNER)
    (nft-transfer? common-collectible token-id sender recipient)
  )
)

;; -------------------------------------------------
;; Treasury
;; -------------------------------------------------

(define-public (withdraw)
  (let ((balance (stx-get-balance (as-contract tx-sender))))
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-OWNER)
    (try! (stx-transfer? balance (as-contract tx-sender) tx-sender))
    (ok true)
  )
)

;; -------------------------------------------------
;; Admin
;; -------------------------------------------------

(define-public (transfer-contract-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-OWNER)
    (var-set contract-owner new-owner)
    (ok true)
  )
)

(define-public (set-mint-paused (paused bool))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-OWNER)
    (var-set mint-paused paused)
    (ok true)
  )
)

(define-public (set-base-uri (new-uri (string-ascii 200)))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-OWNER)
    (asserts! (not (var-get metadata-frozen)) ERR-METADATA-FROZEN)
    (var-set base-uri new-uri)
    (ok true)
  )
)

(define-public (freeze-metadata)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-OWNER)
    (var-set metadata-frozen true)
    (ok true)
  )
)

;; -------------------------------------------------
;; Read-Only Helpers
;; -------------------------------------------------

(define-read-only (get-mint-price)
  (ok MINT-PRICE))

(define-read-only (get-max-supply)
  (ok MAX-SUPPLY))

(define-read-only (get-available-supply)
  (ok (- MAX-SUPPLY (var-get last-token-id))))

(define-read-only (get-royalty-info)
  (ok {recipient: (var-get contract-owner), bps: ROYALTY-BPS}))
