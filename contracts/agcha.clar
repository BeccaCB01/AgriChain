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

;; ===========================================
;; PUBLIC FUNCTIONS - FARMER MANAGEMENT
;; ===========================================

;; Register a new farmer
(define-public (register-farmer (name (string-ascii 50)) (location (string-ascii 100)) (phone (string-ascii 20)))
  (let
    (
      (farmer-id (var-get next-farmer-id))
      (existing-farmer (get-farmer-by-principal tx-sender))
    )
    (begin
      ;; Check contract is active
      (asserts! (is-contract-active) ERR_UNAUTHORIZED)
      ;; Check farmer doesn't already exist
      (asserts! (is-none existing-farmer) ERR_ALREADY_EXISTS)
      ;; Validate inputs
      (asserts! (> (len name) u0) ERR_INVALID_AMOUNT)
      (asserts! (> (len location) u0) ERR_INVALID_AMOUNT)

      ;; Create farmer record
      (map-set farmers farmer-id {
        owner: tx-sender,
        name: name,
        location: location,
        phone: phone,
        verified: false,
        reputation-score: u100, ;; Start with base score
        total-crops-sold: u0,
        registration-block: block-height
      })

      ;; Create principal lookup
      (map-set farmer-principals tx-sender farmer-id)

      ;; Update counters
      (var-set next-farmer-id (+ farmer-id u1))
      (var-set total-farmers (+ (var-get total-farmers) u1))

      (ok farmer-id)
    )
  )
)

;; Update farmer information
(define-public (update-farmer-info (name (string-ascii 50)) (location (string-ascii 100)) (phone (string-ascii 20)))
  (let
    (
      (farmer-id-opt (get-farmer-by-principal tx-sender))
    )
    (begin
      (asserts! (is-contract-active) ERR_UNAUTHORIZED)
      (asserts! (is-some farmer-id-opt) ERR_NOT_FOUND)

      (let
        (
          (farmer-id (unwrap-panic farmer-id-opt))
          (current-farmer (unwrap-panic (map-get? farmers farmer-id)))
        )
        (begin
          ;; Validate inputs
          (asserts! (> (len name) u0) ERR_INVALID_AMOUNT)
          (asserts! (> (len location) u0) ERR_INVALID_AMOUNT)

          ;; Update farmer record
          (map-set farmers farmer-id (merge current-farmer {
            name: name,
            location: location,
            phone: phone
          }))

          (ok farmer-id)
        )
      )
    )
  )
)

;; Verify farmer (admin only)
(define-public (verify-farmer (farmer-id uint))
  (let
    (
      (farmer-opt (map-get? farmers farmer-id))
    )
    (begin
      (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
      (asserts! (is-some farmer-opt) ERR_NOT_FOUND)

      (let
        (
          (farmer (unwrap-panic farmer-opt))
        )
        (begin
          (map-set farmers farmer-id (merge farmer { verified: true }))
          (ok true)
        )
      )
    )
  )
)

;; ===========================================
;; PUBLIC FUNCTIONS - CROP MANAGEMENT
;; ===========================================

;; List a new crop for sale
(define-public (list-crop
  (name (string-ascii 50))
  (variety (string-ascii 50))
  (quantity uint)
  (price-per-kg uint)
  (harvest-date uint)
  (expiry-date uint)
  (description (string-ascii 200))
  (location (string-ascii 100))
)
  (let
    (
      (crop-id (var-get next-crop-id))
      (farmer-id-opt (get-farmer-by-principal tx-sender))
    )
    (begin
      ;; Check contract is active
      (asserts! (is-contract-active) ERR_UNAUTHORIZED)
      ;; Check farmer is registered
      (asserts! (is-some farmer-id-opt) ERR_INVALID_FARMER)
      ;; Validate inputs
      (asserts! (> (len name) u0) ERR_INVALID_AMOUNT)
      (asserts! (> quantity u0) ERR_INVALID_AMOUNT)
      (asserts! (> price-per-kg u0) ERR_INVALID_PRICE)
      (asserts! (> expiry-date harvest-date) ERR_INVALID_AMOUNT)
      (asserts! (> expiry-date block-height) ERR_INVALID_AMOUNT)

      (let
        (
          (farmer-id (unwrap-panic farmer-id-opt))
        )
        (begin
          ;; Create crop record
          (map-set crops crop-id {
            farmer-id: farmer-id,
            name: name,
            variety: variety,
            quantity: quantity,
            price-per-kg: price-per-kg,
            harvest-date: harvest-date,
            expiry-date: expiry-date,
            status: CROP_STATUS_AVAILABLE,
            description: description,
            location: location
          })

          ;; Update counters
          (var-set next-crop-id (+ crop-id u1))
          (var-set total-crops (+ (var-get total-crops) u1))

          (ok crop-id)
        )
      )
    )
  )
)

;; Update crop information (farmer only)
(define-public (update-crop
  (crop-id uint)
  (quantity uint)
  (price-per-kg uint)
  (expiry-date uint)
  (description (string-ascii 200))
)
  (let
    (
      (crop-opt (map-get? crops crop-id))
      (farmer-id-opt (get-farmer-by-principal tx-sender))
    )
    (begin
      (asserts! (is-contract-active) ERR_UNAUTHORIZED)
      (asserts! (is-some crop-opt) ERR_NOT_FOUND)
      (asserts! (is-some farmer-id-opt) ERR_INVALID_FARMER)

      (let
        (
          (crop (unwrap-panic crop-opt))
          (farmer-id (unwrap-panic farmer-id-opt))
        )
        (begin
          ;; Check farmer owns this crop
          (asserts! (is-eq (get farmer-id crop) farmer-id) ERR_UNAUTHORIZED)
          ;; Check crop is still available
          (asserts! (is-eq (get status crop) CROP_STATUS_AVAILABLE) ERR_CROP_NOT_AVAILABLE)
          ;; Validate inputs
          (asserts! (> quantity u0) ERR_INVALID_AMOUNT)
          (asserts! (> price-per-kg u0) ERR_INVALID_PRICE)
          (asserts! (> expiry-date block-height) ERR_INVALID_AMOUNT)

          ;; Update crop record
          (map-set crops crop-id (merge crop {
            quantity: quantity,
            price-per-kg: price-per-kg,
            expiry-date: expiry-date,
            description: description
          }))

          (ok crop-id)
        )
      )
    )
  )
)

;; Remove crop listing (farmer only)
(define-public (remove-crop (crop-id uint))
  (let
    (
      (crop-opt (map-get? crops crop-id))
      (farmer-id-opt (get-farmer-by-principal tx-sender))
    )
    (begin
      (asserts! (is-contract-active) ERR_UNAUTHORIZED)
      (asserts! (is-some crop-opt) ERR_NOT_FOUND)
      (asserts! (is-some farmer-id-opt) ERR_INVALID_FARMER)

      (let
        (
          (crop (unwrap-panic crop-opt))
          (farmer-id (unwrap-panic farmer-id-opt))
        )
        (begin
          ;; Check farmer owns this crop
          (asserts! (is-eq (get farmer-id crop) farmer-id) ERR_UNAUTHORIZED)
          ;; Check crop is still available
          (asserts! (is-eq (get status crop) CROP_STATUS_AVAILABLE) ERR_CROP_NOT_AVAILABLE)

          ;; Update crop status to sold (removing from market)
          (map-set crops crop-id (merge crop { status: CROP_STATUS_SOLD }))

          (ok crop-id)
        )
      )
    )
  )
)

;; ===========================================
;; READ-ONLY FUNCTIONS
;; ===========================================

;; Get farmer information
(define-read-only (get-farmer (farmer-id uint))
  (map-get? farmers farmer-id)
)

;; Get farmer by principal
(define-read-only (get-farmer-id (farmer-principal principal))
  (map-get? farmer-principals farmer-principal)
)

;; Get crop information
(define-read-only (get-crop (crop-id uint))
  (map-get? crops crop-id)
)

;; Get contract statistics
(define-read-only (get-contract-stats)
  {
    total-farmers: (var-get total-farmers),
    total-crops: (var-get total-crops),
    platform-fee-rate: (var-get platform-fee-rate),
    contract-active: (var-get contract-active),
    next-farmer-id: (var-get next-farmer-id),
    next-crop-id: (var-get next-crop-id)
  }
)

;; Check if farmer is verified
(define-read-only (is-farmer-verified (farmer-id uint))
  (match (map-get? farmers farmer-id)
    farmer (get verified farmer)
    false
  )
)

;; Get farmer's reputation score
(define-read-only (get-farmer-reputation (farmer-id uint))
  (match (map-get? farmers farmer-id)
    farmer (get reputation-score farmer)
    u0
  )
)

;; Calculate total crop value
(define-read-only (get-crop-total-value (crop-id uint))
  (match (map-get? crops crop-id)
    crop (* (get quantity crop) (get price-per-kg crop))
    u0
  )
)