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