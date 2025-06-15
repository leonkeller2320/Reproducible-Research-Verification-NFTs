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



(define-map collaboration-invites
    { token-id: uint, invitee: principal }
    {
        inviter: principal,
        timestamp: uint,
        status: (string-ascii 16)
    }
)

(define-map collaborators
    { token-id: uint, collaborator: principal }
    {
        role: (string-ascii 32),
        permissions: uint,
        joined-at: uint,
        contribution-weight: uint
    }
)

(define-map collaboration-settings
    { token-id: uint }
    {
        max-collaborators: uint,
        requires-approval: bool,
        collaboration-open: bool
    }
)

(define-constant err-collaboration-closed (err u106))
(define-constant err-max-collaborators-reached (err u107))
(define-constant err-invite-exists (err u108))
(define-constant err-not-invited (err u109))
(define-constant err-insufficient-permissions (err u110))

(define-constant PERMISSION-EDIT-DATA u1)
(define-constant PERMISSION-INVITE-OTHERS u2)
(define-constant PERMISSION-MANAGE-SETTINGS u4)

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


(define-map research-categories 
    { token-id: uint }
    { 
        primary-category: (string-ascii 32),
        secondary-category: (string-ascii 32),
        keywords: (list 5 (string-ascii 32))
    }
)

(define-public (set-research-categories 
    (token-id uint) 
    (primary (string-ascii 32)) 
    (secondary (string-ascii 32))
    (keywords (list 5 (string-ascii 32))))
    (let ((owner (unwrap! (nft-get-owner? rrv-nft token-id) err-invalid-token)))
        (asserts! (is-eq tx-sender owner) err-not-token-owner)
        (map-set research-categories
            { token-id: token-id }
            {
                primary-category: primary,
                secondary-category: secondary,
                keywords: keywords
            }
        )
        (ok true)
    )
)

(define-read-only (get-research-categories (token-id uint))
    (match (map-get? research-categories { token-id: token-id })
        entry (ok entry)
        (err err-invalid-token)
    )
)


(define-map peer-reviews
    { token-id: uint, reviewer: principal }
    {
        rating: uint,
        review-hash: (string-ascii 64),
        timestamp: uint,
        status: (string-ascii 16)
    }
)

(define-map approved-reviewers
    { reviewer: principal }
    { approved: bool }
)

(define-public (add-reviewer (reviewer principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set approved-reviewers
            { reviewer: reviewer }
            { approved: true }
        )
        (ok true)
    )
)

(define-public (submit-review 
    (token-id uint) 
    (rating uint) 
    (review-hash (string-ascii 64)))
    (let (
        (reviewer-status (default-to { approved: false } 
            (map-get? approved-reviewers { reviewer: tx-sender })))
        )
        (asserts! (get approved reviewer-status) err-not-authorized)
        (asserts! (< rating u6) (err u105))
        (map-set peer-reviews
            { token-id: token-id, reviewer: tx-sender }
            {
                rating: rating,
                review-hash: review-hash,
                timestamp: stacks-block-height,
                status: "completed"
            }
        )
        (ok true)
    )
)

(define-read-only (get-peer-review (token-id uint) (reviewer principal))
    (match (map-get? peer-reviews { token-id: token-id, reviewer: reviewer })
        entry (ok entry)
        (err err-invalid-token)
    )
)


(define-public (enable-collaboration 
    (token-id uint) 
    (max-collaborators uint) 
    (requires-approval bool))
    (let ((owner (unwrap! (nft-get-owner? rrv-nft token-id) err-invalid-token)))
        (asserts! (is-eq tx-sender owner) err-not-token-owner)
        (map-set collaboration-settings
            { token-id: token-id }
            {
                max-collaborators: max-collaborators,
                requires-approval: requires-approval,
                collaboration-open: true
            }
        )
        (map-set collaborators
            { token-id: token-id, collaborator: owner }
            {
                role: "lead-researcher",
                permissions: (+ PERMISSION-EDIT-DATA (+ PERMISSION-INVITE-OTHERS PERMISSION-MANAGE-SETTINGS)),
                joined-at: stacks-block-height,
                contribution-weight: u100
            }
        )
        (ok true)
    )
)

(define-public (invite-collaborator 
    (token-id uint) 
    (invitee principal) 
    (role (string-ascii 32))
    (contribution-weight uint))
    (let (
        (settings (unwrap! (map-get? collaboration-settings { token-id: token-id }) err-collaboration-closed))
        (inviter-perms (default-to { permissions: u0 } 
            (map-get? collaborators { token-id: token-id, collaborator: tx-sender })))
        )
        (asserts! (get collaboration-open settings) err-collaboration-closed)
        (asserts! (> (bit-and (get permissions inviter-perms) PERMISSION-INVITE-OTHERS) u0) err-insufficient-permissions)
        (asserts! (is-none (map-get? collaboration-invites { token-id: token-id, invitee: invitee })) err-invite-exists)
        (map-set collaboration-invites
            { token-id: token-id, invitee: invitee }
            {
                inviter: tx-sender,
                timestamp: stacks-block-height,
                status: "pending"
            }
        )
        (ok true)
    )
)

(define-public (accept-collaboration (token-id uint) (role (string-ascii 32)) (contribution-weight uint))
    (let (
        (invite (unwrap! (map-get? collaboration-invites { token-id: token-id, invitee: tx-sender }) err-not-invited))
        (settings (unwrap! (map-get? collaboration-settings { token-id: token-id }) err-collaboration-closed))
        )
        (asserts! (is-eq (get status invite) "pending") err-not-invited)
        (map-set collaboration-invites
            { token-id: token-id, invitee: tx-sender }
            (merge invite { status: "accepted" })
        )
        (map-set collaborators
            { token-id: token-id, collaborator: tx-sender }
            {
                role: role,
                permissions: PERMISSION-EDIT-DATA,
                joined-at: stacks-block-height,
                contribution-weight: contribution-weight
            }
        )
        (ok true)
    )
)

(define-public (decline-collaboration (token-id uint))
    (let (
        (invite (unwrap! (map-get? collaboration-invites { token-id: token-id, invitee: tx-sender }) err-not-invited))
        )
        (asserts! (is-eq (get status invite) "pending") err-not-invited)
        (map-set collaboration-invites
            { token-id: token-id, invitee: tx-sender }
            (merge invite { status: "declined" })
        )
        (ok true)
    )
)

(define-public (update-research-data-collaborative 
    (token-id uint) 
    (new-methodology (string-ascii 256)) 
    (new-results-hash (string-ascii 64)))
    (let (
        (collaborator-info (unwrap! (map-get? collaborators { token-id: token-id, collaborator: tx-sender }) err-not-authorized))
        (current-data (unwrap! (map-get? research-data { token-id: token-id }) err-invalid-token))
        )
        (asserts! (> (bit-and (get permissions collaborator-info) PERMISSION-EDIT-DATA) u0) err-insufficient-permissions)
        (map-set research-data
            { token-id: token-id }
            (merge current-data {
                methodology: new-methodology,
                results-hash: new-results-hash,
                timestamp: stacks-block-height
            })
        )
        (ok true)
    )
)

(define-public (grant-permissions (token-id uint) (collaborator principal) (new-permissions uint))
    (let (
        (granter-perms (default-to { permissions: u0 } 
            (map-get? collaborators { token-id: token-id, collaborator: tx-sender })))
        (current-collab (unwrap! (map-get? collaborators { token-id: token-id, collaborator: collaborator }) err-not-authorized))
        )
        (asserts! (> (bit-and (get permissions granter-perms) PERMISSION-MANAGE-SETTINGS) u0) err-insufficient-permissions)
        (map-set collaborators
            { token-id: token-id, collaborator: collaborator }
            (merge current-collab { permissions: new-permissions })
        )
        (ok true)
    )
)

(define-read-only (get-collaboration-invite (token-id uint) (invitee principal))
    (match (map-get? collaboration-invites { token-id: token-id, invitee: invitee })
        invite (ok invite)
        (err err-not-invited)
    )
)

(define-read-only (get-collaborator-info (token-id uint) (collaborator principal))
    (match (map-get? collaborators { token-id: token-id, collaborator: collaborator })
        info (ok info)
        (err err-not-authorized)
    )
)

(define-read-only (get-collaboration-settings (token-id uint))
    (match (map-get? collaboration-settings { token-id: token-id })
        settings (ok settings)
        (err err-collaboration-closed)
    )
)

(define-read-only (is-collaborator (token-id uint) (user principal))
    (is-some (map-get? collaborators { token-id: token-id, collaborator: user }))
)

(define-read-only (has-permission (token-id uint) (user principal) (permission uint))
    (match (map-get? collaborators { token-id: token-id, collaborator: user })
        collab (> (bit-and (get permissions collab) permission) u0)
        false
    )
)