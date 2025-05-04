;; Masterfolio DB: Digital Portfolio Management 
;; 
;; This contract establishes a framework for managing creative digital assets
;; Participants can catalog creations, transfer ownership rights, and organize personal libraries
;; Implemented in Clarity for maximum transparency and trustless operation


;; ------------------------------------------------------------
;; System-Wide Constants and  Global Tracking Variables
;; ------------------------------------------------------------
(define-data-var nexus-archive-counter uint u0)  
(define-constant ERR-ACCESS-DENIED (err u306)) 
(define-constant ERR-ADMIN-ONLY (err u307))
(define-constant ERR-FORBIDDEN (err u308))  
(define-constant FRAMEWORK-ADMIN tx-sender)  
(define-data-var nexus-cluster-counter uint u0)   
(define-constant ERR-ITEM-NOT-FOUND (err u301))
(define-constant ERR-DIMENSION-INVALID (err u304)) 
(define-constant ERR-CREDENTIALS-INVALID (err u305)) 
(define-constant ERR-DUPLICATE-ENTRY (err u302)) 
(define-constant ERR-LABEL-INVALID (err u303)) 

;; ------------------------------------------------------------
;; Primary Data Structure Definitions
;; ------------------------------------------------------------


;; Cluster mappings
(define-map cluster-registry
    {cluster-id: uint}  ;; Key: Cluster ID
    {
        name: (string-ascii 64),            ;; Cluster name
        summary: (string-ascii 256),        ;; Cluster description
        originator: principal,              ;; Cluster creator
        genre: (string-ascii 32),           ;; Cluster genre
        inception-block: uint,              ;; Creation block
        update-block: uint,                 ;; Last update block
        item-tally: uint,                   ;; Number of items
        collaborative-mode: bool            ;; Collaboration setting
    }
)

;; Maps for tracking cluster participants
(define-map cluster-participants
    {cluster-id: uint, participant: principal}  ;; Key: Cluster ID and participant
    {
        active-status: bool,       ;; Whether participant status is active
        entry-block: uint,         ;; Block when joined
        originator-flag: bool      ;; Whether participant is originator
    }
)

;; Maps for tracking permission history
(define-map permission-history
    {item-id: uint, grantor: principal, recipient: principal}  ;; Key: Item ID, grantor, and recipient
    {
        grant-block: uint,      ;; Block when access was granted
        revoke-block: uint,     ;; Block when access was revoked (0 if active)
        status-active: bool     ;; Whether access is currently active
    }
)

;; Maps for tracking user feedback
(define-map user-feedback
    {item-id: uint, reviewer: principal}  ;; Key: Item ID and reviewer
    {
        rating: uint,                         ;; Rating (1-5)
        notes: (optional (string-ascii 256)), ;; Optional commentary
        last-update-block: uint,              ;; Block when last updated
        first-review-block: uint              ;; Block when first reviewed
    }
)

;; Maps for tracking aggregated feedback data
(define-map feedback-metrics
    {item-id: uint}  ;; Key: Item ID
    {
        review-count: uint,        ;; Total number of reviews
        latest-review-block: uint  ;; Block when last reviewed
    }
)

;; Maps for tracking user library counters
(define-map curator-library-counters
    {curator: principal}  ;; Key: Curator Principal
    {recent-library-id: uint}  ;; Value: Most recent library created
)

;; Maps for tracking libraries created by users
(define-map creative-libraries
    {curator: principal, library-id: uint}  ;; Key: Library owner and ID
    {
        header: (string-ascii 64),           ;; Library title
        description: (string-ascii 128),     ;; Library description
        genesis-block: uint,                 ;; Creation block
        last-modified-block: uint,           ;; Last modification block
        item-count: uint,                    ;; Number of items
        visibility-flag: bool                ;; Visibility setting
    }
)

;; Maps for tracking items within libraries
(define-map library-items
    {library-curator: principal, library-id: uint, item-id: uint}  ;; Key: Library curator, ID, and item ID
    {
        inclusion-block: uint,  ;; Block when added
        display-order: uint     ;; Position in library
    }
)

;; Central registry for all creative items
(define-map nexus-archive
    {item-id: uint}  ;; Key: Unique item identifier
    {
        title: (string-ascii 64),           ;; Item title (max 64 chars)
        creator: (string-ascii 32),         ;; Creator name (max 32 chars)
        rights-owner: principal,            ;; Current rights owner address
        duration-value: uint,               ;; Content length in units
        registration-timestamp: uint,       ;; Block height at registration
        classification: (string-ascii 32),  ;; Content classification
        attribute-tags: (list 8 (string-ascii 24))  ;; Descriptive tags for search and organization
    }
)

;; Maps for tracking items in clusters
(define-map cluster-items
    {cluster-id: uint, item-id: uint}  ;; Key: Cluster ID and item ID
    {
        contributor: principal,     ;; Contributor who added item
        inclusion-block: uint       ;; Block when added
    }
)

;; Maps for tracking access permissions
(define-map item-permissions
    {item-id: uint, viewer: principal}  ;; Key: Item ID and User Principal
    {permission-granted: bool}  ;; Value: Whether access is permitted
)


;; ------------------------------------------------------------
;; Helper Utility Functions (Private)
;; ------------------------------------------------------------

;; Checks if an item exists in the archive
(define-private (item-exists (item-id uint))
    (is-some (map-get? nexus-archive {item-id: item-id}))
)

;; Verifies ownership status for an item
(define-private (is-rights-owner (item-id uint) (user principal))
    (match (map-get? nexus-archive {item-id: item-id})
        item-data (is-eq (get rights-owner item-data) user)
        false
    )
)

;; Retrieves item duration value
(define-private (get-item-duration (item-id uint))
    (default-to u0 
        (get duration-value 
            (map-get? nexus-archive {item-id: item-id})
        )
    )
)

;; Validates that an attribute tag meets length requirements
(define-private (is-valid-tag (tag (string-ascii 24)))
    (and 
        (> (len tag) u0)
        (< (len tag) u25)
    )
)

;; Validates that a set of attribute tags meets system requirements
(define-private (are-valid-tags (tags (list 8 (string-ascii 24))))
    (and
        (> (len tags) u0)
        (<= (len tags) u8)
        (is-eq (len (filter is-valid-tag tags)) (len tags))
    )
)

;; Retrieves most recent library identifier for a curator
(define-private (get-recent-library-id (curator principal))
    (get recent-library-id (default-to {recent-library-id: u0} 
        (map-get? curator-library-counters {curator: curator})))
)

;; Prepares item identifiers for bulk operations
(define-private (prepare-item-id (item-id uint))
    {item-id: item-id}
)

;; Adds item to cluster during bulk operations
(define-private (add-item-to-cluster (item-data {item-id: uint}))
    (let
        ((item-id (get item-id item-data)))
        (and 
            (item-exists item-id)
            (map-insert cluster-items
                {cluster-id: (var-get nexus-cluster-counter), item-id: item-id}
                {
                    contributor: tx-sender,
                    inclusion-block: block-height
                }
            )
        )
    )
)

;; ------------------------------------------------------------
;; Core Public Functions
;; ------------------------------------------------------------

;; Registers a new item in the archive
(define-public (register-new-item 
        (title (string-ascii 64))
        (creator (string-ascii 32))
        (duration-value uint)
        (classification (string-ascii 32))
        (tags (list 8 (string-ascii 24)))
    )
    (let
        ((new-item-id (+ (var-get nexus-archive-counter) u1)))

        ;; Input validation
        (asserts! (and (> (len title) u0) (< (len title) u65)) ERR-LABEL-INVALID)
        (asserts! (and (> (len creator) u0) (< (len creator) u33)) ERR-LABEL-INVALID)
        (asserts! (and (> duration-value u0) (< duration-value u10000)) ERR-DIMENSION-INVALID)
        (asserts! (and (> (len classification) u0) (< (len classification) u33)) ERR-LABEL-INVALID)
        (asserts! (are-valid-tags tags) ERR-LABEL-INVALID)

        ;; Add item to archive
        (map-insert nexus-archive
            {item-id: new-item-id}
            {
                title: title,
                creator: creator,
                rights-owner: tx-sender,
                duration-value: duration-value,
                registration-timestamp: block-height,
                classification: classification,
                attribute-tags: tags
            }
        )

        ;; Establish initial permissions
        (map-insert item-permissions
            {item-id: new-item-id, viewer: tx-sender}
            {permission-granted: true}
        )

        ;; Update archive counter and return new identifier
        (var-set nexus-archive-counter new-item-id)
        (ok new-item-id)
    )
)

;; Removes an item from the archive
(define-public (remove-archived-item (item-id uint))
    (let
        ((item-data (unwrap! (map-get? nexus-archive {item-id: item-id}) ERR-ITEM-NOT-FOUND)))

        ;; Validation
        (asserts! (item-exists item-id) ERR-ITEM-NOT-FOUND)
        (asserts! (is-eq (get rights-owner item-data) tx-sender) ERR-CREDENTIALS-INVALID)

        ;; Remove item data
        (map-delete nexus-archive {item-id: item-id})
        (map-delete item-permissions {item-id: item-id, viewer: tx-sender})
        (ok true)
    )
)

;; Transfers item ownership to a new owner
(define-public (transfer-item-ownership (item-id uint) (new-owner principal))
    (let
        ((item-data (unwrap! (map-get? nexus-archive {item-id: item-id}) ERR-ITEM-NOT-FOUND)))

        ;; Validation
        (asserts! (item-exists item-id) ERR-ITEM-NOT-FOUND)
        (asserts! (is-eq (get rights-owner item-data) tx-sender) ERR-CREDENTIALS-INVALID)

        ;; Update rights owner
        (map-set nexus-archive
            {item-id: item-id}
            (merge item-data {rights-owner: new-owner})
        )
        (ok true)
    )
)

;; Updates item details
(define-public (update-item-details 
        (item-id uint) 
        (new-title (string-ascii 64)) 
        (new-duration-value uint) 
        (new-classification (string-ascii 32)) 
        (new-tags (list 8 (string-ascii 24)))
    )
    (let
        ((item-data (unwrap! (map-get? nexus-archive {item-id: item-id}) ERR-ITEM-NOT-FOUND)))

        ;; Validation
        (asserts! (item-exists item-id) ERR-ITEM-NOT-FOUND)
        (asserts! (is-eq (get rights-owner item-data) tx-sender) ERR-CREDENTIALS-INVALID)
        (asserts! (and (> (len new-title) u0) (< (len new-title) u65)) ERR-LABEL-INVALID)
        (asserts! (and (> new-duration-value u0) (< new-duration-value u10000)) ERR-DIMENSION-INVALID)
        (asserts! (and (> (len new-classification) u0) (< (len new-classification) u33)) ERR-LABEL-INVALID)
        (asserts! (are-valid-tags new-tags) ERR-LABEL-INVALID)

        ;; Update item details
        (map-set nexus-archive
            {item-id: item-id}
            (merge item-data {
                title: new-title,
                duration-value: new-duration-value,
                classification: new-classification,
                attribute-tags: new-tags
            })
        )
        (ok true)
    )
)

;; Adds item to personal library
(define-public (add-to-personal-library 
        (library-id uint)
        (item-id uint)
    )
    (let
        ((library-data (unwrap! (map-get? creative-libraries {curator: tx-sender, library-id: library-id}) ERR-ITEM-NOT-FOUND))
         (item-data (unwrap! (map-get? nexus-archive {item-id: item-id}) ERR-ITEM-NOT-FOUND))
         (viewer-access (default-to {permission-granted: false} (map-get? item-permissions {item-id: item-id, viewer: tx-sender}))))

        ;; Validation
        (asserts! (item-exists item-id) ERR-ITEM-NOT-FOUND)
        (asserts! (or 
                    (is-eq (get rights-owner item-data) tx-sender)
                    (get permission-granted viewer-access)
                  ) 
                ERR-ACCESS-DENIED)

        ;; Check for duplicates
        (asserts! (is-none (map-get? library-items {library-curator: tx-sender, library-id: library-id, item-id: item-id})) 
                 ERR-DUPLICATE-ENTRY)

        (ok true)
    )
)

;; Grants access to an item
(define-public (grant-item-access 
        (item-id uint)
        (recipient principal)
    )
    (let
        ((item-data (unwrap! (map-get? nexus-archive {item-id: item-id}) ERR-ITEM-NOT-FOUND)))

        ;; Validation
        (asserts! (item-exists item-id) ERR-ITEM-NOT-FOUND)
        (asserts! (is-eq (get rights-owner item-data) tx-sender) ERR-CREDENTIALS-INVALID)
        (asserts! (not (is-eq tx-sender recipient)) ERR-LABEL-INVALID)

        ;; Check for existing grant
        (asserts! (is-none (map-get? item-permissions {item-id: item-id, viewer: recipient})) 
                 ERR-DUPLICATE-ENTRY)

        ;; Grant access
        (map-insert item-permissions
            {item-id: item-id, viewer: recipient}
            {permission-granted: true}
        )

        ;; Record grant history
        (map-insert permission-history
            {item-id: item-id, grantor: tx-sender, recipient: recipient}
            {
                grant-block: block-height,
                revoke-block: u0,
                status-active: true
            }
        )

        (ok true)
    )
)

;; Revokes previously granted access
(define-public (revoke-item-access 
        (item-id uint)
        (recipient principal)
    )
    (let
        ((item-data (unwrap! (map-get? nexus-archive {item-id: item-id}) ERR-ITEM-NOT-FOUND))
         (access-data (unwrap! (map-get? permission-history {item-id: item-id, grantor: tx-sender, recipient: recipient}) ERR-ITEM-NOT-FOUND)))

        ;; Validation
        (asserts! (item-exists item-id) ERR-ITEM-NOT-FOUND)
        (asserts! (is-eq (get rights-owner item-data) tx-sender) ERR-CREDENTIALS-INVALID)
        (asserts! (get status-active access-data) ERR-ACCESS-DENIED)

        (ok true)
    )
)

;; Submits user feedback for an item
(define-public (submit-item-feedback 
        (item-id uint)
        (rating-score uint)
        (review-notes (optional (string-ascii 256)))
    )
    (let
        ((item-data (unwrap! (map-get? nexus-archive {item-id: item-id}) ERR-ITEM-NOT-FOUND))
         (viewer-access (default-to {permission-granted: false} (map-get? item-permissions {item-id: item-id, viewer: tx-sender})))
         (existing-feedback (map-get? user-feedback {item-id: item-id, reviewer: tx-sender})))

        ;; Validation
        (asserts! (item-exists item-id) ERR-ITEM-NOT-FOUND)
        (asserts! (or 
                    (is-eq (get rights-owner item-data) tx-sender)
                    (get permission-granted viewer-access)
                  ) 
                ERR-ACCESS-DENIED)
        (asserts! (and (>= rating-score u1) (<= rating-score u5)) ERR-LABEL-INVALID)

        ;; Validate review notes length if provided
        (if (is-some review-notes)
            (asserts! (and 
                        (> (len (default-to "" review-notes)) u0) 
                        (< (len (default-to "" review-notes)) u257)
                      ) 
                    ERR-LABEL-INVALID)
            true
        )

        ;; Store or update feedback
        (if (is-some existing-feedback)
            ;; Update existing feedback
            (map-set user-feedback
                {item-id: item-id, reviewer: tx-sender}
                {
                    rating: rating-score,
                    notes: review-notes,
                    last-update-block: block-height,
                    first-review-block: (get first-review-block (unwrap! existing-feedback ERR-ITEM-NOT-FOUND))
                }
            )
            ;; Create new feedback
            (map-insert user-feedback
                {item-id: item-id, reviewer: tx-sender}
                {
                    rating: rating-score,
                    notes: review-notes,
                    last-update-block: block-height,
                    first-review-block: block-height
                }
            )
        )

        ;; Update feedback metrics
        (match (map-get? feedback-metrics {item-id: item-id})
            existing-metrics (map-set feedback-metrics
                {item-id: item-id}
                (merge existing-metrics {
                    review-count: (if (is-some existing-feedback) 
                                      (get review-count existing-metrics) 
                                      (+ (get review-count existing-metrics) u1)),
                    latest-review-block: block-height
                })
            )
            (map-insert feedback-metrics
                {item-id: item-id}
                {
                    review-count: u1,
                    latest-review-block: block-height
                }
            )
        )

        (ok true)
    )
)

;; Creates a themed cluster of items
(define-public (create-themed-cluster
        (cluster-name (string-ascii 64))
        (cluster-summary (string-ascii 256))
        (genre-type (string-ascii 32))
        (initial-items (list 20 uint))
        (allow-collaboration bool)
    )
    (let
        ((new-cluster-id (+ (var-get nexus-cluster-counter) u1))
         (validated-items (filter item-exists initial-items)))

        ;; Validation
        (asserts! (and (> (len cluster-name) u0) (< (len cluster-name) u65)) ERR-LABEL-INVALID)
        (asserts! (and (> (len cluster-summary) u0) (< (len cluster-summary) u257)) ERR-LABEL-INVALID)
        (asserts! (and (> (len genre-type) u0) (< (len genre-type) u33)) ERR-LABEL-INVALID)

        ;; Register originator as participant
        (map-insert cluster-participants
            {cluster-id: new-cluster-id, participant: tx-sender}
            {
                active-status: true,
                entry-block: block-height,
                originator-flag: true
            }
        )

        ;; Add validated items to cluster
        (map add-item-to-cluster (map prepare-item-id validated-items))

        ;; Update cluster registry size
        (var-set nexus-cluster-counter new-cluster-id)

        (ok new-cluster-id)
    )
)

