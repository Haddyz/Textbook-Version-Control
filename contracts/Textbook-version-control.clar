(define-trait textbook-trait
  (
    (get-metadata (uint) (response {title: (string-ascii 256), description: (string-ascii 512)} uint))
  )
)

(define-non-fungible-token textbook uint)

(define-constant ERR-NOT-AUTHORIZED (err u1001))
(define-constant ERR-NOT-FOUND (err u1002))
(define-constant ERR-ALREADY-EXISTS (err u1003))
(define-constant ERR-INVALID-VERSION (err u1004))
(define-constant ERR-NOT-SUBSCRIBED (err u1005))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u1006))
(define-constant ERR-INVALID-RATING (err u1007))
(define-constant ERR-ALREADY-REVIEWED (err u1008))
(define-constant ERR-NOT-COAUTHOR (err u1009))
(define-constant ERR-INVALID-PERCENTAGE (err u1010))
(define-constant ERR-ALREADY-COAUTHOR (err u1011))
(define-constant ERR-CANNOT-REMOVE-LEAD (err u1012))
(define-constant ERR-ALREADY-BOOKMARKED (err u1013))
(define-constant ERR-NOT-BOOKMARKED (err u1014))
(define-constant ERR-NO-ACTIVE-PROMO (err u1015))
(define-constant ERR-PROMO-EXPIRED (err u1016))
(define-constant ERR-INVALID-DISCOUNT (err u1017))
(define-constant ERR-GIFT-ALREADY-CLAIMED (err u1018))
(define-constant ERR-GIFT-NOT-FOUND (err u1019))
(define-constant ERR-CANNOT-GIFT-SELF (err u1020))

(define-data-var next-textbook-id uint u1)
(define-data-var subscription-price uint u1000000)

(define-map textbooks uint {
  title: (string-ascii 256),
  description: (string-ascii 512),
  author: principal,
  current-version: uint,
  created-at: uint,
  price: uint,
  active: bool
})

(define-map textbook-versions {textbook-id: uint, version: uint} {
  content-hash: (string-ascii 64),
  changelog: (string-ascii 512),
  released-at: uint,
  size-bytes: uint
})

(define-map student-subscriptions {student: principal, textbook-id: uint} {
  subscribed-at: uint,
  last-sync-version: uint,
  active: bool,
  auto-update: bool
})

(define-map author-earnings principal uint)

(define-map textbook-stats uint {
  total-subscribers: uint,
  total-downloads: uint,
  total-earnings: uint,
  total-reviews: uint,
  average-rating: uint
})

(define-map textbook-reviews {textbook-id: uint, reviewer: principal} {
  rating: uint,
  review-text: (string-ascii 500),
  created-at: uint
})

(define-map textbook-coauthors {textbook-id: uint, coauthor: principal} {
  contribution-percentage: uint,
  role: (string-ascii 50),
  can-publish-updates: bool,
  added-at: uint,
  total-earned: uint
})

(define-map textbook-collaboration-info uint {
  total-coauthors: uint,
  revenue-pool: uint,
  last-distribution: uint,
  collaboration-active: bool
})

(define-map coauthor-invites {textbook-id: uint, invitee: principal} {
  contribution-percentage: uint,
  role: (string-ascii 50),
  can-publish-updates: bool,
  invited-at: uint,
  invited-by: principal
})

(define-map user-bookmarks {user: principal, textbook-id: uint} {
  bookmarked-at: uint,
  notes: (string-ascii 500)
})

(define-map textbook-bookmark-counts uint uint)

(define-map textbook-promotions uint {
  discount-percentage: uint,
  start-block: uint,
  end-block: uint,
  promo-code: (string-ascii 20),
  max-uses: uint,
  current-uses: uint,
  active: bool
})

(define-data-var next-gift-id uint u1)

(define-map textbook-gifts uint {
  gift-id: uint,
  textbook-id: uint,
  sender: principal,
  recipient: principal,
  message: (string-ascii 200),
  purchased-at: uint,
  claimed: bool,
  auto-update: bool
})

(define-public (create-textbook (title (string-ascii 256)) (description (string-ascii 512)) (initial-content-hash (string-ascii 64)) (price uint))
  (let 
    (
      (textbook-id (var-get next-textbook-id))
    )
    (try! (nft-mint? textbook textbook-id tx-sender))
    
    (map-set textbooks textbook-id {
      title: title,
      description: description,
      author: tx-sender,
      current-version: u1,
      created-at: stacks-block-height,
      price: price,
      active: true
    })
    
    (map-set textbook-versions {textbook-id: textbook-id, version: u1} {
      content-hash: initial-content-hash,
      changelog: "Initial version",
      released-at: stacks-block-height,
      size-bytes: u0
    })
    
    (map-set textbook-stats textbook-id {
      total-subscribers: u0,
      total-downloads: u0,
      total-earnings: u0,
      total-reviews: u0,
      average-rating: u0
    })
    
    (map-set textbook-coauthors {textbook-id: textbook-id, coauthor: tx-sender} {
      contribution-percentage: u10000,
      role: "Lead Author",
      can-publish-updates: true,
      added-at: stacks-block-height,
      total-earned: u0
    })
    
    (map-set textbook-collaboration-info textbook-id {
      total-coauthors: u1,
      revenue-pool: u0,
      last-distribution: stacks-block-height,
      collaboration-active: false
    })
    
    (var-set next-textbook-id (+ textbook-id u1))
    (ok textbook-id)
  )
)

(define-public (publish-update (textbook-id uint) (content-hash (string-ascii 64)) (changelog (string-ascii 512)) (size-bytes uint))
  (let 
    (
      (textbook-data (unwrap! (map-get? textbooks textbook-id) ERR-NOT-FOUND))
      (new-version (+ (get current-version textbook-data) u1))
      (coauthor-data (map-get? textbook-coauthors {textbook-id: textbook-id, coauthor: tx-sender}))
    )
    (asserts! (or 
      (is-eq (get author textbook-data) tx-sender)
      (match coauthor-data
        coauthor (get can-publish-updates coauthor)
        false
      )
    ) ERR-NOT-AUTHORIZED)
    (asserts! (get active textbook-data) ERR-NOT-FOUND)
    
    (map-set textbooks textbook-id 
      (merge textbook-data {current-version: new-version}))
    
    (map-set textbook-versions {textbook-id: textbook-id, version: new-version} {
      content-hash: content-hash,
      changelog: changelog,
      released-at: stacks-block-height,
      size-bytes: size-bytes
    })
    
    (ok new-version)
  )
)

(define-public (subscribe-to-textbook (textbook-id uint) (auto-update bool))
  (let 
    (
      (textbook-data (unwrap! (map-get? textbooks textbook-id) ERR-NOT-FOUND))
      (payment-amount (get price textbook-data))
      (stats (unwrap! (map-get? textbook-stats textbook-id) ERR-NOT-FOUND))
      (collab-info (map-get? textbook-collaboration-info textbook-id))
    )
    (asserts! (get active textbook-data) ERR-NOT-FOUND)
    (asserts! (>= (stx-get-balance tx-sender) payment-amount) ERR-INSUFFICIENT-PAYMENT)
    
    (if (match collab-info
          info (get collaboration-active info)
          false)
      (try! (stx-transfer? payment-amount tx-sender (as-contract tx-sender)))
      (try! (stx-transfer? payment-amount tx-sender (get author textbook-data)))
    )
    
    (map-set student-subscriptions {student: tx-sender, textbook-id: textbook-id} {
      subscribed-at: stacks-block-height,
      last-sync-version: (get current-version textbook-data),
      active: true,
      auto-update: auto-update
    })
    
    (map-set textbook-stats textbook-id 
      (merge stats {
        total-subscribers: (+ (get total-subscribers stats) u1),
        total-earnings: (+ (get total-earnings stats) payment-amount)
      }))
    
    (match collab-info
      info (if (get collaboration-active info)
        (map-set textbook-collaboration-info textbook-id
          (merge info {revenue-pool: (+ (get revenue-pool info) payment-amount)}))
        (map-set author-earnings (get author textbook-data)
          (+ (default-to u0 (map-get? author-earnings (get author textbook-data))) payment-amount))
      )
      (map-set author-earnings (get author textbook-data)
        (+ (default-to u0 (map-get? author-earnings (get author textbook-data))) payment-amount))
    )
    
    (ok true)
  )
)

(define-public (sync-textbook (textbook-id uint))
  (let 
    (
      (subscription (unwrap! (map-get? student-subscriptions {student: tx-sender, textbook-id: textbook-id}) ERR-NOT-SUBSCRIBED))
      (textbook-data (unwrap! (map-get? textbooks textbook-id) ERR-NOT-FOUND))
      (stats (unwrap! (map-get? textbook-stats textbook-id) ERR-NOT-FOUND))
      (current-version (get current-version textbook-data))
      (last-sync (get last-sync-version subscription))
    )
    (asserts! (get active subscription) ERR-NOT-SUBSCRIBED)
    (asserts! (< last-sync current-version) ERR-INVALID-VERSION)
    
    (map-set student-subscriptions {student: tx-sender, textbook-id: textbook-id}
      (merge subscription {last-sync-version: current-version}))
    
    (map-set textbook-stats textbook-id 
      (merge stats {total-downloads: (+ (get total-downloads stats) u1)}))
    
    (ok current-version)
  )
)

(define-public (unsubscribe-from-textbook (textbook-id uint))
  (let 
    (
      (subscription (unwrap! (map-get? student-subscriptions {student: tx-sender, textbook-id: textbook-id}) ERR-NOT-SUBSCRIBED))
      (stats (unwrap! (map-get? textbook-stats textbook-id) ERR-NOT-FOUND))
    )
    (map-set student-subscriptions {student: tx-sender, textbook-id: textbook-id}
      (merge subscription {active: false}))
    
    (map-set textbook-stats textbook-id 
      (merge stats {total-subscribers: (- (get total-subscribers stats) u1)}))
    
    (ok true)
  )
)

(define-public (toggle-auto-update (textbook-id uint))
  (let 
    (
      (subscription (unwrap! (map-get? student-subscriptions {student: tx-sender, textbook-id: textbook-id}) ERR-NOT-SUBSCRIBED))
    )
    (asserts! (get active subscription) ERR-NOT-SUBSCRIBED)
    
    (map-set student-subscriptions {student: tx-sender, textbook-id: textbook-id}
      (merge subscription {auto-update: (not (get auto-update subscription))}))
    
    (ok (not (get auto-update subscription)))
  )
)

(define-public (deactivate-textbook (textbook-id uint))
  (let 
    (
      (textbook-data (unwrap! (map-get? textbooks textbook-id) ERR-NOT-FOUND))
    )
    (asserts! (is-eq (get author textbook-data) tx-sender) ERR-NOT-AUTHORIZED)
    
    (map-set textbooks textbook-id 
      (merge textbook-data {active: false}))
    
    (ok true)
  )
)

(define-public (update-subscription-price (new-price uint))
  (begin
    (asserts! (is-eq tx-sender contract-caller) ERR-NOT-AUTHORIZED)
    (var-set subscription-price new-price)
    (ok new-price)
  )
)

(define-read-only (get-textbook (textbook-id uint))
  (map-get? textbooks textbook-id)
)

(define-read-only (get-textbook-version (textbook-id uint) (version uint))
  (map-get? textbook-versions {textbook-id: textbook-id, version: version})
)

(define-read-only (get-latest-version (textbook-id uint))
  (match (map-get? textbooks textbook-id)
    textbook-data (map-get? textbook-versions {textbook-id: textbook-id, version: (get current-version textbook-data)})
    none
  )
)

(define-read-only (get-subscription (student principal) (textbook-id uint))
  (map-get? student-subscriptions {student: student, textbook-id: textbook-id})
)

(define-read-only (get-textbook-stats (textbook-id uint))
  (map-get? textbook-stats textbook-id)
)

(define-read-only (get-author-earnings (author principal))
  (default-to u0 (map-get? author-earnings author))
)

(define-read-only (get-pending-updates (student principal) (textbook-id uint))
  (match (map-get? student-subscriptions {student: student, textbook-id: textbook-id})
    subscription 
      (match (map-get? textbooks textbook-id)
        textbook-data 
          (if (and (get active subscription) (< (get last-sync-version subscription) (get current-version textbook-data)))
            (some (- (get current-version textbook-data) (get last-sync-version subscription)))
            none
          )
        none
      )
    none
  )
)

(define-read-only (is-subscribed (student principal) (textbook-id uint))
  (match (map-get? student-subscriptions {student: student, textbook-id: textbook-id})
    subscription (get active subscription)
    false
  )
)

(define-read-only (get-subscription-price)
  (var-get subscription-price)
)

(define-read-only (list-versions (textbook-id uint) (start-version uint) (end-version uint))
  (let 
    (
      (textbook-data (unwrap! (map-get? textbooks textbook-id) none))
      (max-version (get current-version textbook-data))
    )
    (if (and (<= start-version end-version) (<= end-version max-version))
      (some {
        start: start-version,
        end: end-version,
        total-versions: max-version
      })
      none
    )
  )
)

(define-public (submit-review (textbook-id uint) (rating uint) (review-text (string-ascii 500)))
  (let 
    (
      (existing-review (map-get? textbook-reviews {textbook-id: textbook-id, reviewer: tx-sender}))
      (subscription (unwrap! (map-get? student-subscriptions {student: tx-sender, textbook-id: textbook-id}) ERR-NOT-SUBSCRIBED))
      (textbook-data (unwrap! (map-get? textbooks textbook-id) ERR-NOT-FOUND))
      (current-stats (unwrap! (map-get? textbook-stats textbook-id) ERR-NOT-FOUND))
      (current-total (get total-reviews current-stats))
      (current-avg (get average-rating current-stats))
      (new-total (+ current-total u1))
      (new-avg (/ (+ (* current-avg current-total) rating) new-total))
    )
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)
    (asserts! (get active subscription) ERR-NOT-SUBSCRIBED)
    (asserts! (is-none existing-review) ERR-ALREADY-REVIEWED)
    
    (map-set textbook-reviews {textbook-id: textbook-id, reviewer: tx-sender} {
      rating: rating,
      review-text: review-text,
      created-at: stacks-block-height
    })
    
    (map-set textbook-stats textbook-id 
      (merge current-stats {
        total-reviews: new-total,
        average-rating: new-avg
      }))
    
    (ok true)
  )
)

(define-public (update-review (textbook-id uint) (rating uint) (review-text (string-ascii 500)))
  (let 
    (
      (existing-review (unwrap! (map-get? textbook-reviews {textbook-id: textbook-id, reviewer: tx-sender}) ERR-NOT-FOUND))
      (subscription (unwrap! (map-get? student-subscriptions {student: tx-sender, textbook-id: textbook-id}) ERR-NOT-SUBSCRIBED))
      (current-stats (unwrap! (map-get? textbook-stats textbook-id) ERR-NOT-FOUND))
      (current-total (get total-reviews current-stats))
      (current-avg (get average-rating current-stats))
      (old-rating (get rating existing-review))
      (new-avg (/ (+ (- (* current-avg current-total) old-rating) rating) current-total))
    )
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)
    (asserts! (get active subscription) ERR-NOT-SUBSCRIBED)
    
    (map-set textbook-reviews {textbook-id: textbook-id, reviewer: tx-sender} {
      rating: rating,
      review-text: review-text,
      created-at: (get created-at existing-review)
    })
    
    (map-set textbook-stats textbook-id 
      (merge current-stats {average-rating: new-avg}))
    
    (ok true)
  )
)

(define-read-only (get-textbook-review (textbook-id uint) (reviewer principal))
  (map-get? textbook-reviews {textbook-id: textbook-id, reviewer: reviewer})
)

(define-read-only (get-textbook-rating (textbook-id uint))
  (match (map-get? textbook-stats textbook-id)
    stats (some {
      average-rating: (get average-rating stats),
      total-reviews: (get total-reviews stats)
    })
    none
  )
)

(define-public (invite-coauthor (textbook-id uint) (coauthor principal) (contribution-percentage uint) (role (string-ascii 50)) (can-publish bool))
  (let
    (
      (textbook-data (unwrap! (map-get? textbooks textbook-id) ERR-NOT-FOUND))
      (existing-coauthor (map-get? textbook-coauthors {textbook-id: textbook-id, coauthor: coauthor}))
      (existing-invite (map-get? coauthor-invites {textbook-id: textbook-id, invitee: coauthor}))
    )
    (asserts! (is-eq (get author textbook-data) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-none existing-coauthor) ERR-ALREADY-COAUTHOR)
    (asserts! (and (> contribution-percentage u0) (<= contribution-percentage u10000)) ERR-INVALID-PERCENTAGE)
    
    (map-set coauthor-invites {textbook-id: textbook-id, invitee: coauthor} {
      contribution-percentage: contribution-percentage,
      role: role,
      can-publish-updates: can-publish,
      invited-at: stacks-block-height,
      invited-by: tx-sender
    })
    
    (ok true)
  )
)

(define-public (accept-coauthor-invite (textbook-id uint))
  (let
    (
      (invite (unwrap! (map-get? coauthor-invites {textbook-id: textbook-id, invitee: tx-sender}) ERR-NOT-FOUND))
      (collab-info (unwrap! (map-get? textbook-collaboration-info textbook-id) ERR-NOT-FOUND))
    )
    
    (map-set textbook-coauthors {textbook-id: textbook-id, coauthor: tx-sender} {
      contribution-percentage: (get contribution-percentage invite),
      role: (get role invite),
      can-publish-updates: (get can-publish-updates invite),
      added-at: stacks-block-height,
      total-earned: u0
    })
    
    (map-set textbook-collaboration-info textbook-id
      (merge collab-info {
        total-coauthors: (+ (get total-coauthors collab-info) u1),
        collaboration-active: true
      }))
    
    (map-delete coauthor-invites {textbook-id: textbook-id, invitee: tx-sender})
    
    (ok true)
  )
)

(define-public (distribute-revenue (textbook-id uint))
  (let
    (
      (textbook-data (unwrap! (map-get? textbooks textbook-id) ERR-NOT-FOUND))
      (collab-info (unwrap! (map-get? textbook-collaboration-info textbook-id) ERR-NOT-FOUND))
      (revenue-pool (get revenue-pool collab-info))
    )
    (asserts! (is-eq (get author textbook-data) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (get collaboration-active collab-info) ERR-NOT-FOUND)
    (asserts! (> revenue-pool u0) ERR-INSUFFICIENT-PAYMENT)
    
    (map-set textbook-collaboration-info textbook-id
      (merge collab-info {
        revenue-pool: u0,
        last-distribution: stacks-block-height
      }))
    
    (ok revenue-pool)
  )
)

(define-public (claim-coauthor-revenue (textbook-id uint))
  (let
    (
      (coauthor-data (unwrap! (map-get? textbook-coauthors {textbook-id: textbook-id, coauthor: tx-sender}) ERR-NOT-COAUTHOR))
      (collab-info (unwrap! (map-get? textbook-collaboration-info textbook-id) ERR-NOT-FOUND))
      (last-distribution (get last-distribution collab-info))
      (revenue-pool (get revenue-pool collab-info))
      (share-amount (/ (* revenue-pool (get contribution-percentage coauthor-data)) u10000))
    )
    (asserts! (> share-amount u0) ERR-INSUFFICIENT-PAYMENT)
    
    (try! (as-contract (stx-transfer? share-amount tx-sender tx-sender)))
    
    (map-set textbook-coauthors {textbook-id: textbook-id, coauthor: tx-sender}
      (merge coauthor-data {
        total-earned: (+ (get total-earned coauthor-data) share-amount)
      }))
    
    (map-set textbook-collaboration-info textbook-id
      (merge collab-info {
        revenue-pool: (- revenue-pool share-amount)
      }))
    
    (ok share-amount)
  )
)

(define-public (update-coauthor-permissions (textbook-id uint) (coauthor principal) (can-publish bool))
  (let
    (
      (textbook-data (unwrap! (map-get? textbooks textbook-id) ERR-NOT-FOUND))
      (coauthor-data (unwrap! (map-get? textbook-coauthors {textbook-id: textbook-id, coauthor: coauthor}) ERR-NOT-COAUTHOR))
    )
    (asserts! (is-eq (get author textbook-data) tx-sender) ERR-NOT-AUTHORIZED)
    
    (map-set textbook-coauthors {textbook-id: textbook-id, coauthor: coauthor}
      (merge coauthor-data {can-publish-updates: can-publish}))
    
    (ok true)
  )
)

(define-public (remove-coauthor (textbook-id uint) (coauthor principal))
  (let
    (
      (textbook-data (unwrap! (map-get? textbooks textbook-id) ERR-NOT-FOUND))
      (coauthor-data (unwrap! (map-get? textbook-coauthors {textbook-id: textbook-id, coauthor: coauthor}) ERR-NOT-COAUTHOR))
      (collab-info (unwrap! (map-get? textbook-collaboration-info textbook-id) ERR-NOT-FOUND))
    )
    (asserts! (is-eq (get author textbook-data) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (not (is-eq coauthor (get author textbook-data))) ERR-CANNOT-REMOVE-LEAD)
    
    (map-delete textbook-coauthors {textbook-id: textbook-id, coauthor: coauthor})
    
    (map-set textbook-collaboration-info textbook-id
      (merge collab-info {
        total-coauthors: (- (get total-coauthors collab-info) u1),
        collaboration-active: (> (- (get total-coauthors collab-info) u1) u1)
      }))
    
    (ok true)
  )
)

(define-read-only (get-coauthor-info (textbook-id uint) (coauthor principal))
  (map-get? textbook-coauthors {textbook-id: textbook-id, coauthor: coauthor})
)

(define-read-only (get-collaboration-info (textbook-id uint))
  (map-get? textbook-collaboration-info textbook-id)
)

(define-read-only (get-coauthor-invite (textbook-id uint) (invitee principal))
  (map-get? coauthor-invites {textbook-id: textbook-id, invitee: invitee})
)

(define-read-only (is-coauthor (textbook-id uint) (user principal))
  (is-some (map-get? textbook-coauthors {textbook-id: textbook-id, coauthor: user}))
)

(define-public (bookmark-textbook (textbook-id uint) (notes (string-ascii 500)))
  (let
    (
      (existing-bookmark (map-get? user-bookmarks {user: tx-sender, textbook-id: textbook-id}))
      (textbook-data (unwrap! (map-get? textbooks textbook-id) ERR-NOT-FOUND))
      (current-count (default-to u0 (map-get? textbook-bookmark-counts textbook-id)))
    )
    (asserts! (get active textbook-data) ERR-NOT-FOUND)
    (asserts! (is-none existing-bookmark) ERR-ALREADY-BOOKMARKED)
    
    (map-set user-bookmarks {user: tx-sender, textbook-id: textbook-id} {
      bookmarked-at: stacks-block-height,
      notes: notes
    })
    
    (map-set textbook-bookmark-counts textbook-id (+ current-count u1))
    
    (ok true)
  )
)

(define-public (remove-bookmark (textbook-id uint))
  (let
    (
      (bookmark (unwrap! (map-get? user-bookmarks {user: tx-sender, textbook-id: textbook-id}) ERR-NOT-BOOKMARKED))
      (current-count (default-to u0 (map-get? textbook-bookmark-counts textbook-id)))
    )
    (map-delete user-bookmarks {user: tx-sender, textbook-id: textbook-id})
    
    (map-set textbook-bookmark-counts textbook-id (- current-count u1))
    
    (ok true)
  )
)

(define-public (update-bookmark-notes (textbook-id uint) (notes (string-ascii 500)))
  (let
    (
      (bookmark (unwrap! (map-get? user-bookmarks {user: tx-sender, textbook-id: textbook-id}) ERR-NOT-BOOKMARKED))
    )
    (map-set user-bookmarks {user: tx-sender, textbook-id: textbook-id}
      (merge bookmark {notes: notes}))
    
    (ok true)
  )
)

(define-read-only (get-user-bookmark (user principal) (textbook-id uint))
  (map-get? user-bookmarks {user: user, textbook-id: textbook-id})
)

(define-read-only (is-bookmarked (user principal) (textbook-id uint))
  (is-some (map-get? user-bookmarks {user: user, textbook-id: textbook-id}))
)

(define-read-only (get-bookmark-count (textbook-id uint))
  (default-to u0 (map-get? textbook-bookmark-counts textbook-id))
)

(define-public (create-promotion (textbook-id uint) (discount-percentage uint) (duration-blocks uint) (promo-code (string-ascii 20)) (max-uses uint))
  (let
    (
      (textbook-data (unwrap! (map-get? textbooks textbook-id) ERR-NOT-FOUND))
    )
    (asserts! (is-eq (get author textbook-data) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (and (> discount-percentage u0) (<= discount-percentage u9000)) ERR-INVALID-DISCOUNT)
    (asserts! (get active textbook-data) ERR-NOT-FOUND)
    
    (map-set textbook-promotions textbook-id {
      discount-percentage: discount-percentage,
      start-block: stacks-block-height,
      end-block: (+ stacks-block-height duration-blocks),
      promo-code: promo-code,
      max-uses: max-uses,
      current-uses: u0,
      active: true
    })
    
    (ok true)
  )
)

(define-public (subscribe-with-promo (textbook-id uint) (auto-update bool) (promo-code (string-ascii 20)))
  (let
    (
      (textbook-data (unwrap! (map-get? textbooks textbook-id) ERR-NOT-FOUND))
      (promo (unwrap! (map-get? textbook-promotions textbook-id) ERR-NO-ACTIVE-PROMO))
      (original-price (get price textbook-data))
      (discount-amount (/ (* original-price (get discount-percentage promo)) u10000))
      (discounted-price (- original-price discount-amount))
      (stats (unwrap! (map-get? textbook-stats textbook-id) ERR-NOT-FOUND))
      (collab-info (map-get? textbook-collaboration-info textbook-id))
    )
    (asserts! (get active textbook-data) ERR-NOT-FOUND)
    (asserts! (get active promo) ERR-NO-ACTIVE-PROMO)
    (asserts! (<= stacks-block-height (get end-block promo)) ERR-PROMO-EXPIRED)
    (asserts! (is-eq promo-code (get promo-code promo)) ERR-NO-ACTIVE-PROMO)
    (asserts! (< (get current-uses promo) (get max-uses promo)) ERR-NO-ACTIVE-PROMO)
    (asserts! (>= (stx-get-balance tx-sender) discounted-price) ERR-INSUFFICIENT-PAYMENT)
    
    (if (match collab-info
          info (get collaboration-active info)
          false)
      (try! (stx-transfer? discounted-price tx-sender (as-contract tx-sender)))
      (try! (stx-transfer? discounted-price tx-sender (get author textbook-data)))
    )
    
    (map-set student-subscriptions {student: tx-sender, textbook-id: textbook-id} {
      subscribed-at: stacks-block-height,
      last-sync-version: (get current-version textbook-data),
      active: true,
      auto-update: auto-update
    })
    
    (map-set textbook-stats textbook-id 
      (merge stats {
        total-subscribers: (+ (get total-subscribers stats) u1),
        total-earnings: (+ (get total-earnings stats) discounted-price)
      }))
    
    (map-set textbook-promotions textbook-id
      (merge promo {current-uses: (+ (get current-uses promo) u1)}))
    
    (match collab-info
      info (if (get collaboration-active info)
        (map-set textbook-collaboration-info textbook-id
          (merge info {revenue-pool: (+ (get revenue-pool info) discounted-price)}))
        (map-set author-earnings (get author textbook-data)
          (+ (default-to u0 (map-get? author-earnings (get author textbook-data))) discounted-price))
      )
      (map-set author-earnings (get author textbook-data)
        (+ (default-to u0 (map-get? author-earnings (get author textbook-data))) discounted-price))
    )
    
    (ok discounted-price)
  )
)

(define-public (end-promotion (textbook-id uint))
  (let
    (
      (textbook-data (unwrap! (map-get? textbooks textbook-id) ERR-NOT-FOUND))
      (promo (unwrap! (map-get? textbook-promotions textbook-id) ERR-NO-ACTIVE-PROMO))
    )
    (asserts! (is-eq (get author textbook-data) tx-sender) ERR-NOT-AUTHORIZED)
    
    (map-set textbook-promotions textbook-id
      (merge promo {active: false}))
    
    (ok true)
  )
)

(define-read-only (get-promotion (textbook-id uint))
  (map-get? textbook-promotions textbook-id)
)

(define-read-only (get-discounted-price (textbook-id uint))
  (match (map-get? textbooks textbook-id)
    textbook-data (match (map-get? textbook-promotions textbook-id)
      promo (if (and (get active promo) (<= stacks-block-height (get end-block promo)) (< (get current-uses promo) (get max-uses promo)))
        (let
          (
            (original-price (get price textbook-data))
            (discount-amount (/ (* original-price (get discount-percentage promo)) u10000))
          )
          (some (- original-price discount-amount))
        )
        (some (get price textbook-data))
      )
      (some (get price textbook-data))
    )
    none
  )
)

(define-read-only (is-promo-active (textbook-id uint))
  (match (map-get? textbook-promotions textbook-id)
    promo (and (get active promo) (<= stacks-block-height (get end-block promo)) (< (get current-uses promo) (get max-uses promo)))
    false
  )
)

(define-public (gift-textbook (textbook-id uint) (recipient principal) (message (string-ascii 200)) (auto-update bool))
  (let
    (
      (textbook-data (unwrap! (map-get? textbooks textbook-id) ERR-NOT-FOUND))
      (payment-amount (get price textbook-data))
      (stats (unwrap! (map-get? textbook-stats textbook-id) ERR-NOT-FOUND))
      (gift-id (var-get next-gift-id))
      (collab-info (map-get? textbook-collaboration-info textbook-id))
    )
    (asserts! (get active textbook-data) ERR-NOT-FOUND)
    (asserts! (not (is-eq tx-sender recipient)) ERR-CANNOT-GIFT-SELF)
    (asserts! (>= (stx-get-balance tx-sender) payment-amount) ERR-INSUFFICIENT-PAYMENT)
    
    (if (match collab-info
          info (get collaboration-active info)
          false)
      (try! (stx-transfer? payment-amount tx-sender (as-contract tx-sender)))
      (try! (stx-transfer? payment-amount tx-sender (get author textbook-data)))
    )
    
    (map-set textbook-gifts gift-id {
      gift-id: gift-id,
      textbook-id: textbook-id,
      sender: tx-sender,
      recipient: recipient,
      message: message,
      purchased-at: stacks-block-height,
      claimed: false,
      auto-update: auto-update
    })
    
    (map-set textbook-stats textbook-id 
      (merge stats {
        total-earnings: (+ (get total-earnings stats) payment-amount)
      }))
    
    (match collab-info
      info (if (get collaboration-active info)
        (map-set textbook-collaboration-info textbook-id
          (merge info {revenue-pool: (+ (get revenue-pool info) payment-amount)}))
        (map-set author-earnings (get author textbook-data)
          (+ (default-to u0 (map-get? author-earnings (get author textbook-data))) payment-amount))
      )
      (map-set author-earnings (get author textbook-data)
        (+ (default-to u0 (map-get? author-earnings (get author textbook-data))) payment-amount))
    )
    
    (var-set next-gift-id (+ gift-id u1))
    (ok gift-id)
  )
)

(define-public (claim-gift (gift-id uint))
  (let
    (
      (gift-data (unwrap! (map-get? textbook-gifts gift-id) ERR-GIFT-NOT-FOUND))
      (textbook-data (unwrap! (map-get? textbooks (get textbook-id gift-data)) ERR-NOT-FOUND))
      (stats (unwrap! (map-get? textbook-stats (get textbook-id gift-data)) ERR-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (get recipient gift-data)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get claimed gift-data)) ERR-GIFT-ALREADY-CLAIMED)
    (asserts! (get active textbook-data) ERR-NOT-FOUND)
    
    (map-set student-subscriptions {student: tx-sender, textbook-id: (get textbook-id gift-data)} {
      subscribed-at: stacks-block-height,
      last-sync-version: (get current-version textbook-data),
      active: true,
      auto-update: (get auto-update gift-data)
    })
    
    (map-set textbook-gifts gift-id
      (merge gift-data {claimed: true}))
    
    (map-set textbook-stats (get textbook-id gift-data) 
      (merge stats {
        total-subscribers: (+ (get total-subscribers stats) u1)
      }))
    
    (ok (get textbook-id gift-data))
  )
)

(define-read-only (get-gift (gift-id uint))
  (map-get? textbook-gifts gift-id)
)

(define-read-only (is-gift-claimed (gift-id uint))
  (match (map-get? textbook-gifts gift-id)
    gift-data (get claimed gift-data)
    false
  )
)

(define-read-only (get-gift-sender (gift-id uint))
  (match (map-get? textbook-gifts gift-id)
    gift-data (some (get sender gift-data))
    none
  )
)
