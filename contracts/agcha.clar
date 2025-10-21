;; AgriChain Smart Contract
;; A decentralized marketplace for smallholder farmers to tokenize and sell their crops directly
;; Ensures transparent pricing and reduces exploitation by middlemen

;; ===========================================
;; CONSTANTS & ERROR CODES
;; ===========================================

;; Contract owner
(define-constant CONTRACT_OWNER tx-sender)

;; Error codes
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))
(define-constant ERR_INSUFFICIENT_BALANCE (err u104))
(define-constant ERR_INVALID_FARMER (err u105))
(define-constant ERR_CROP_NOT_AVAILABLE (err u106))
(define-constant ERR_INVALID_PRICE (err u107))

;; Crop status constants
(define-constant CROP_STATUS_AVAILABLE u1)
(define-constant CROP_STATUS_SOLD u2)
(define-constant CROP_STATUS_RESERVED u3)

;; ===========================================
;; DATA VARIABLES
;; ===========================================

;; Global counters
(define-data-var next-farmer-id uint u1)
(define-data-var next-crop-id uint u1)
(define-data-var total-farmers uint u0)
(define-data-var total-crops uint u0)

;; Contract settings
(define-data-var contract-active bool true)
(define-data-var platform-fee-rate uint u250) ;; 2.5% in basis points (250/10000)

;; ===========================================
;; DATA MAPS
;; ===========================================

;; Farmer registry
(define-map farmers
  uint ;; farmer-id
  {
    owner: principal,
    name: (string-ascii 50),
    location: (string-ascii 100),
    phone: (string-ascii 20),
    verified: bool,
    reputation-score: uint,
    total-crops-sold: uint,
    registration-block: uint
  }
)

;; Farmer lookup by principal
(define-map farmer-principals
  principal ;; farmer address
  uint      ;; farmer-id
)

;; Crop registry
(define-map crops
  uint ;; crop-id
  {
    farmer-id: uint,
    name: (string-ascii 50),
    variety: (string-ascii 50),
    quantity: uint, ;; in kg
    price-per-kg: uint, ;; in microSTX
    harvest-date: uint, ;; block height
    expiry-date: uint,  ;; block height
    status: uint,
    description: (string-ascii 200),
    location: (string-ascii 100)
  }
)

;; ===========================================
;; PRIVATE FUNCTIONS
;; ===========================================

;; Check if caller is contract owner
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT_OWNER)
)

;; Check if contract is active
(define-private (is-contract-active)
  (var-get contract-active)
)

;; Validate farmer exists
(define-private (farmer-exists (farmer-id uint))
  (is-some (map-get? farmers farmer-id))
)

;; Get farmer by principal
(define-private (get-farmer-by-principal (farmer-principal principal))
  (map-get? farmer-principals farmer-principal)
)

;; ===========================================
;; PUBLIC FUNCTIONS - ADMIN
;; ===========================================

;; Toggle contract active status (admin only)
(define-public (toggle-contract-status)
  (begin
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    (var-set contract-active (not (var-get contract-active)))
    (ok (var-get contract-active))
  )
)

;; Update platform fee rate (admin only)
(define-public (update-platform-fee (new-rate uint))
  (begin
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    (asserts! (<= new-rate u1000) ERR_INVALID_AMOUNT) ;; Max 10%
    (var-set platform-fee-rate new-rate)
    (ok new-rate)
  )
)