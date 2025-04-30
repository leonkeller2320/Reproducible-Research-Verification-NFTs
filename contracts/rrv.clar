;; (impl-trait 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait.nft-trait)

(define-non-fungible-token rrv-nft uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-token-exists (err u102))
(define-constant err-invalid-token (err u103))
(define-constant err-not-authorized (err u104))

(define-data-var token-id-nonce uint u0)
(define-data-var base-uri (string-ascii 256) "ipfs://")

(define-map token-uris { token-id: uint } { uri: (string-ascii 256) })
(define-map research-data 
    { token-id: uint }
    {
        dataset-hash: (string-ascii 64),
        methodology: (string-ascii 256),
        results-hash: (string-ascii 64),
        timestamp: uint,
        researcher: principal,
        verified: bool
    }
)

(define-map token-verifiers { token-id: uint, verifier: principal } { verified: bool })

(define-read-only (get-last-token-id)
    (var-get token-id-nonce)
)

(define-read-only (get-token-uri (token-id uint))
    (match (map-get? token-uris { token-id: token-id })
        entry (ok (get uri entry))
        (err err-invalid-token)
    )
)

(define-read-only (get-owner (token-id uint))
    (ok (nft-get-owner? rrv-nft token-id))
)

(define-read-only (get-research-data (token-id uint))
    (match (map-get? research-data { token-id: token-id })
        entry (ok entry)
        (err err-invalid-token)
    )
)

(define-public (mint (dataset-hash (string-ascii 64)) (methodology (string-ascii 256)) (results-hash (string-ascii 64)))
    (let
        (
            (token-id (+ (var-get token-id-nonce) u1))
            (researcher tx-sender)
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (try! (nft-mint? rrv-nft token-id researcher))
        (map-set research-data
            { token-id: token-id }
            {
                dataset-hash: dataset-hash,
                methodology: methodology,
                results-hash: results-hash,
                timestamp: stacks-block-height,
                researcher: researcher,
                verified: false
            }
        )
        (var-set token-id-nonce token-id)
        (ok token-id)
    )
)

(define-public (verify-research (token-id uint))
    (let
        (
            (verifier tx-sender)
        )
        (asserts! (is-some (nft-get-owner? rrv-nft token-id)) err-invalid-token)
        (map-set token-verifiers
            { token-id: token-id, verifier: verifier }
            { verified: true }
        )
        (ok true)
    )
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender sender) err-not-authorized)
        (nft-transfer? rrv-nft token-id sender recipient)
    )
)

(define-read-only (get-verification-status (token-id uint) (verifier principal))
    (match (map-get? token-verifiers { token-id: token-id, verifier: verifier })
        entry (ok (get verified entry))
        (ok false)
    )
)

(define-public (set-base-uri (new-base-uri (string-ascii 256)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set base-uri new-base-uri)
        (ok true)
    )
)