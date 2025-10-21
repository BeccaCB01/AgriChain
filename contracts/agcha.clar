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
;; ===========================================
;; ADDITIONAL DATA MAPS FOR TRADING
;; ===========================================

;; Purchase orders
(define-map purchase-orders
  uint ;; order-id
  {
    buyer: principal,
    crop-id: uint,
    quantity: uint,
    total-price: uint,
    status: uint, ;; 1=pending, 2=confirmed, 3=completed, 4=cancelled
    created-at: uint,
    escrow-amount: uint
  }
)

;; Order counter
(define-data-var next-order-id uint u1)

;; Order status constants
(define-constant ORDER_STATUS_PENDING u1)
(define-constant ORDER_STATUS_CONFIRMED u2)
(define-constant ORDER_STATUS_COMPLETED u3)
(define-constant ORDER_STATUS_CANCELLED u4)

;; Escrow balances
(define-map escrow-balances
  principal ;; buyer
  uint      ;; amount in microSTX
)

;; ===========================================
;; PUBLIC FUNCTIONS - MARKETPLACE TRADING
;; ===========================================

;; Create a purchase order
(define-public (create-purchase-order (crop-id uint) (quantity uint))
  (let
    (
      (crop-opt (map-get? crops crop-id))
      (order-id (var-get next-order-id))
    )
    (begin
      (asserts! (is-contract-active) ERR_UNAUTHORIZED)
      (asserts! (is-some crop-opt) ERR_NOT_FOUND)
      (asserts! (> quantity u0) ERR_INVALID_AMOUNT)

      (let
        (
          (crop (unwrap-panic crop-opt))
          (farmer-opt (map-get? farmers (get farmer-id crop)))
        )
        (begin
          ;; Check crop is available
          (asserts! (is-eq (get status crop) CROP_STATUS_AVAILABLE) ERR_CROP_NOT_AVAILABLE)
          ;; Check quantity is available
          (asserts! (<= quantity (get quantity crop)) ERR_INVALID_AMOUNT)
          ;; Check buyer is not the farmer
          (asserts! (not (is-eq tx-sender (get owner (unwrap-panic farmer-opt)))) ERR_UNAUTHORIZED)
          ;; Check crop hasn't expired
          (asserts! (> (get expiry-date crop) block-height) ERR_CROP_NOT_AVAILABLE)

          (let
            (
              (total-price (* quantity (get price-per-kg crop)))
              (platform-fee (/ (* total-price (var-get platform-fee-rate)) u10000))
              (escrow-amount (+ total-price platform-fee))
            )
            (begin
              ;; Transfer STX to escrow
              (try! (stx-transfer? escrow-amount tx-sender (as-contract tx-sender)))

              ;; Update escrow balance
              (map-set escrow-balances tx-sender
                (+ (default-to u0 (map-get? escrow-balances tx-sender)) escrow-amount))

              ;; Create purchase order
              (map-set purchase-orders order-id {
                buyer: tx-sender,
                crop-id: crop-id,
                quantity: quantity,
                total-price: total-price,
                status: ORDER_STATUS_PENDING,
                created-at: block-height,
                escrow-amount: escrow-amount
              })

              ;; Update order counter
              (var-set next-order-id (+ order-id u1))

              (ok order-id)
            )
          )
        )
      )
    )
  )
)

;; Confirm purchase order (farmer only)
(define-public (confirm-purchase-order (order-id uint))
  (let
    (
      (order-opt (map-get? purchase-orders order-id))
      (farmer-id-opt (get-farmer-by-principal tx-sender))
    )
    (begin
      (asserts! (is-contract-active) ERR_UNAUTHORIZED)
      (asserts! (is-some order-opt) ERR_NOT_FOUND)
      (asserts! (is-some farmer-id-opt) ERR_INVALID_FARMER)

      (let
        (
          (order (unwrap-panic order-opt))
          (farmer-id (unwrap-panic farmer-id-opt))
          (crop-opt (map-get? crops (get crop-id order)))
        )
        (begin
          (asserts! (is-some crop-opt) ERR_NOT_FOUND)

          (let
            (
              (crop (unwrap-panic crop-opt))
            )
            (begin
              ;; Check farmer owns the crop
              (asserts! (is-eq (get farmer-id crop) farmer-id) ERR_UNAUTHORIZED)
              ;; Check order is pending
              (asserts! (is-eq (get status order) ORDER_STATUS_PENDING) ERR_INVALID_AMOUNT)
              ;; Check crop is still available
              (asserts! (is-eq (get status crop) CROP_STATUS_AVAILABLE) ERR_CROP_NOT_AVAILABLE)

              ;; Update order status
              (map-set purchase-orders order-id (merge order { status: ORDER_STATUS_CONFIRMED }))

              ;; Reserve the crop
              (map-set crops (get crop-id order) (merge crop { status: CROP_STATUS_RESERVED }))

              (ok order-id)
            )
          )
        )
      )
    )
  )
)

;; Complete purchase order (buyer only - after receiving goods)
(define-public (complete-purchase-order (order-id uint))
  (let
    (
      (order-opt (map-get? purchase-orders order-id))
    )
    (begin
      (asserts! (is-contract-active) ERR_UNAUTHORIZED)
      (asserts! (is-some order-opt) ERR_NOT_FOUND)

      (let
        (
          (order (unwrap-panic order-opt))
          (crop-opt (map-get? crops (get crop-id order)))
        )
        (begin
          (asserts! (is-some crop-opt) ERR_NOT_FOUND)
          ;; Check buyer is calling
          (asserts! (is-eq tx-sender (get buyer order)) ERR_UNAUTHORIZED)
          ;; Check order is confirmed
          (asserts! (is-eq (get status order) ORDER_STATUS_CONFIRMED) ERR_INVALID_AMOUNT)

          (let
            (
              (crop (unwrap-panic crop-opt))
              (farmer-opt (map-get? farmers (get farmer-id crop)))
              (platform-fee (/ (* (get total-price order) (var-get platform-fee-rate)) u10000))
              (farmer-payment (- (get total-price order) platform-fee))
            )
            (begin
              (asserts! (is-some farmer-opt) ERR_NOT_FOUND)

              (let
                (
                  (farmer (unwrap-panic farmer-opt))
                  (current-escrow (default-to u0 (map-get? escrow-balances tx-sender)))
                )
                (begin
                  ;; Check escrow balance
                  (asserts! (>= current-escrow (get escrow-amount order)) ERR_INSUFFICIENT_BALANCE)

                  ;; Transfer payment to farmer
                  (try! (as-contract (stx-transfer? farmer-payment tx-sender (get owner farmer))))

                  ;; Transfer platform fee to contract owner
                  (try! (as-contract (stx-transfer? platform-fee tx-sender CONTRACT_OWNER)))

                  ;; Update escrow balance
                  (map-set escrow-balances tx-sender (- current-escrow (get escrow-amount order)))

                  ;; Update order status
                  (map-set purchase-orders order-id (merge order { status: ORDER_STATUS_COMPLETED }))

                  ;; Update crop status and quantity
                  (if (is-eq (get quantity order) (get quantity crop))
                    ;; Entire crop sold
                    (map-set crops (get crop-id order) (merge crop {
                      status: CROP_STATUS_SOLD,
                      quantity: u0
                    }))
                    ;; Partial sale
                    (map-set crops (get crop-id order) (merge crop {
                      status: CROP_STATUS_AVAILABLE,
                      quantity: (- (get quantity crop) (get quantity order))
                    }))
                  )

                  ;; Update farmer stats
                  (map-set farmers (get farmer-id crop) (merge farmer {
                    total-crops-sold: (+ (get total-crops-sold farmer) u1),
                    reputation-score: (+ (get reputation-score farmer) u10) ;; Increase reputation
                  }))

                  (ok order-id)
                )
              )
            )
          )
        )
      )
    )
  )
)

;; Cancel purchase order
(define-public (cancel-purchase-order (order-id uint))
  (let
    (
      (order-opt (map-get? purchase-orders order-id))
    )
    (begin
      (asserts! (is-contract-active) ERR_UNAUTHORIZED)
      (asserts! (is-some order-opt) ERR_NOT_FOUND)

      (let
        (
          (order (unwrap-panic order-opt))
          (crop-opt (map-get? crops (get crop-id order)))
        )
        (begin
          (asserts! (is-some crop-opt) ERR_NOT_FOUND)

          (let
            (
              (crop (unwrap-panic crop-opt))
              (farmer-opt (map-get? farmers (get farmer-id crop)))
              (is-buyer (is-eq tx-sender (get buyer order)))
              (is-farmer (and (is-some farmer-opt)
                             (is-eq tx-sender (get owner (unwrap-panic farmer-opt)))))
            )
            (begin
              ;; Check caller is buyer or farmer
              (asserts! (or is-buyer is-farmer) ERR_UNAUTHORIZED)
              ;; Check order is pending or confirmed
              (asserts! (or (is-eq (get status order) ORDER_STATUS_PENDING)
                           (is-eq (get status order) ORDER_STATUS_CONFIRMED)) ERR_INVALID_AMOUNT)

              (let
                (
                  (current-escrow (default-to u0 (map-get? escrow-balances (get buyer order))))
                )
                (begin
                  ;; Refund escrow to buyer
                  (if (> current-escrow u0)
                    (begin
                      (try! (as-contract (stx-transfer? (get escrow-amount order) tx-sender (get buyer order))))
                      (map-set escrow-balances (get buyer order) (- current-escrow (get escrow-amount order)))
                    )
                    true
                  )

                  ;; Update order status
                  (map-set purchase-orders order-id (merge order { status: ORDER_STATUS_CANCELLED }))

                  ;; Release crop reservation if it was reserved
                  (if (is-eq (get status crop) CROP_STATUS_RESERVED)
                    (map-set crops (get crop-id order) (merge crop { status: CROP_STATUS_AVAILABLE }))
                    true
                  )

                  (ok order-id)
                )
              )
            )
          )
        )
      )
    )
  )
)

;; ===========================================
;; READ-ONLY FUNCTIONS - MARKETPLACE
;; ===========================================

;; Get purchase order information
(define-read-only (get-purchase-order (order-id uint))
  (map-get? purchase-orders order-id)
)

;; Get buyer's escrow balance
(define-read-only (get-escrow-balance (buyer principal))
  (default-to u0 (map-get? escrow-balances buyer))
)

;; Calculate platform fee for an amount
(define-read-only (calculate-platform-fee (amount uint))
  (/ (* amount (var-get platform-fee-rate)) u10000)
)

;; Get next order ID
(define-read-only (get-next-order-id)
  (var-get next-order-id)
)

;; Check if crop is available for purchase
(define-read-only (is-crop-available (crop-id uint))
  (match (map-get? crops crop-id)
    crop (and
           (is-eq (get status crop) CROP_STATUS_AVAILABLE)
           (> (get expiry-date crop) block-height)
           (> (get quantity crop) u0))
    false
  )
)

;; Get marketplace summary
(define-read-only (get-marketplace-summary)
  {
    total-orders: (- (var-get next-order-id) u1),
    platform-fee-rate: (var-get platform-fee-rate),
    contract-active: (var-get contract-active)
  }
)