#lang racket
(require csv-reading)

(define filename "Video Games Sales.csv")

;;Reader for reading CSV file
(define (csvfile->list filename)
  (call-with-input-file filename csv->list))

;;converts CSV file to list
(define list(csvfile->list filename))

;;drop first element
(define mylist(rest list))

;; Define a struct for video game sales data
(struct V (index rank title platform year genre publisher NA EU JP R.world Global Review))

;; Function to create VideoGameSale instances from a sublist
(define (create-video-game-sale game-data)
  (apply V game-data))

;; Map the list of lists to VideoGameSale instances
(define video-game-sales-data
  (map create-video-game-sale mylist))


;;return t or false 
(define (ask-yes-or-no question)
  (display question)
  (let* ([response (read-line)]
         [normalized-response (string-trim response)])
    (cond
      [(string=? normalized-response "yes") #t]
      [(string=? normalized-response "no") #f]
      [else (ask-yes-or-no "Please enter 'yes' or 'no': ")])))



;;Function to filter by name
(define (filter-by-Name name games)
  (filter (lambda (game) (string-contains? (string-downcase(V-title game)) (string-downcase name)))
          games))
;;Function to filter by Date
(define (filter-by-Date date1 date2 games)
  (filter (lambda (game) (and (<= date1 (string->number(V-year game)))(>= date2 (string->number(V-year game)))))
          games))
;; Function to filter by publisher
(define (filter-by-publisher publisher games)
  (filter (lambda (game) (string-contains? (string-downcase(V-publisher game)) (string-downcase publisher)))
          games))

;;Function to filter by Genre
(define (filter-by-Genre Genre games)
  (filter (lambda (game) (string=? (V-genre game) Genre))
          games))

 
;;Function to sort by Review

  (define (sort-by-review lst)
    (sort lst
        (lambda (e1 e2)
        (>(string->number (V-Review e1))
          (string->number (V-Review e2))))))



;;Function to get name from user
(define (get-name)
  (displayln "Enter Game Title")
  (define name (read-line))
  name)
 ;;Prompt user for publisher name
(define (get-publisher)
 (displayln "Enter Publisher")
 (define publisher (read-line))
   publisher)

;;Function to get dates
(define (get-valid-year)
  (let loop ()
    (display "Enter a year: ")
    (define input (read-line))
    (if (and (string? input) ; Check if input is a string
             (= (string-length input) 4) ; Check if input has four characters
             (regexp-match #px"^[0-9]{4}$" input)) ; Check if input is all digits
        (string->number input) ; Convert string to number
        (begin
          (displayln "Invalid input. Please enter a four-digit year.")
          (loop))))) 

;;Function to get genre
(define (get-genre)
  (let loop ()
    (displayln "Enter Category of Game:")
    (displayln "Role-Playing Strategy Fighting Adventure Shooter Action Sports Platform Racing Puzzle Simulation Misc")
    (define genre (read-line))
    (if (member genre '("Role-Playing" "Strategy" "Fighting" "Adventure" "Shooter" "Action" "Sports" "Platform" "Racing" "Puzzle" "Simulation" "Misc"))
        genre
        (begin
          (displayln "Invalid genre. Please enter a valid category.")
          (loop)))))

;;Function to get region
(define (get-region)
  (displayln "Choose a region to filter results for:")
  (displayln "1. NA")
  (displayln "2. EU")
  (displayln "3. JP")
  (displayln "4. Rest of the world")
  (displayln "5. Global")
  (displayln "6. None")
  (let* ([choice (read)]
         [regions '("NA" "EU" "JP" "Rest of the world" "Global" "None")])
    (cond
      [(and (integer? choice) (<= 1 choice 6))
       (displayln (format "Selected region: ~a" (list-ref regions (- choice 1))))]
      [else
       (displayln "Invalid choice. Please select a valid option.")]
      )
    choice)
  )

(define (filter-1 data)
  (define user-response (ask-yes-or-no "Do you want to filter by name? (yes/no): "))
    (cond
      [user-response (filter-by-Name (get-name) data)]
      [else data]))

(define (filter-2 data)
  (define user-response (ask-yes-or-no "Do you want to filter by Date? (yes/no): "))
    (cond
      [user-response (filter-by-Date (get-valid-year)(get-valid-year)data)]
      [else data]))

(define (filter-3 data)
  (define user-response (ask-yes-or-no "Do you want to filter by publisher? (yes/no): "))
    (cond
      [user-response (filter-by-publisher (get-publisher) data)]
      [else data]))

(define (filter-4 data)
  (define user-response (ask-yes-or-no "Do you want to filter genre? (yes/no): "))
    (cond
      [user-response (filter-by-Genre (get-genre) data)]
      [else data]))
(define (sorted data)
  (define user-response (ask-yes-or-no "Do you want sort by review? Sales Rank is Default (yes/no): "))
    (cond
      [user-response (sort-by-review data)]
      [else data]))



;;print results
(define (showresults-NA mystruct)
  (for ([game mystruct])
  (printf "Rank: ~a, Title: ~a, Platform: ~a,Year: ~a,Genre: ~a, Publisher: ~a, Sales (NA): ~a, Review: ~a\n\n"
          (V-rank game)
          (V-title game)
          (V-platform game)
          (V-year game)
          (V-genre game)
          (V-publisher game)
          (V-NA game)
          (V-Review game))))

(define (showresults-EU mystruct)
  (for ([game mystruct])
  (printf "Rank: ~a, Title: ~a, Platform: ~a,Year: ~a,Genre: ~a, Publisher: ~a, Sales (EU): ~a, Review: ~a\n\n"
          (V-rank game)
          (V-title game)
          (V-platform game)
          (V-year game)
          (V-genre game)
          (V-publisher game)
          (V-EU game)
          (V-Review game))))

(define (showresults-JP mystruct)
  (for ([game mystruct])
  (printf "Rank: ~a, Title: ~a, Platform: ~a,Year: ~a,Genre: ~a, Publisher: ~a, Sales (JP): ~a, Review: ~a\n\n"
          (V-rank game)
          (V-title game)
          (V-platform game)
          (V-year game)
          (V-genre game)
          (V-publisher game)
          (V-JP game)
          (V-Review game))))

(define (showresults-ROW mystruct)
  (for ([game mystruct])
  (printf "Rank: ~a, Title: ~a, Platform: ~a,Year: ~a,Genre: ~a, Publisher: ~a, Sales (rest of the world): ~a, Review: ~a\n\n"
          (V-rank game)
          (V-title game)
          (V-platform game)
          (V-year game)
          (V-genre game)
          (V-publisher game)
          (V-R.world game)
          (V-Review game))))

(define (showresults-Global mystruct)
  (for ([game mystruct])
  (printf "Rank: ~a, Title: ~a, Platform: ~a,Year: ~a,Genre: ~a, Publisher: ~a, Sales (Global): ~a, Review: ~a\n\n"
          (V-rank game)
          (V-title game)
          (V-platform game)
          (V-year game)
          (V-genre game)
          (V-publisher game)
          (V-Global game)
          (V-Review game))))

(define (showresults-all mystruct)
  (for ([game mystruct])
  (printf "Rank: ~a, Title: ~a, Platform: ~a,Year: ~a,Genre: ~a, Publisher: ~a, Sales (NA): ~a, Sales (EU): ~a,Sales (JP): ~a, Sales (rest of the world): ~a, Sales (Global): ~a, Review: ~a\n\n"
          (V-rank game)
          (V-title game)
          (V-platform game)
          (V-year game)
          (V-genre game)
          (V-publisher game)
          (V-NA game)
          (V-EU game)
          (V-JP game)
          (V-R.world game)
          (V-Global game)
          (V-Review game))))


(define (filter-5 data)
  (define user-response (ask-yes-or-no "Do you want to filter by Region? (yes/no): "))
    (cond
      [user-response (define x (get-region))
                     (cond
                       [(= x 1) (showresults-NA data)]
                       [(= x 2) (showresults-EU data)]
                       [(= x 3) (showresults-JP data)]
                       [(= x 4) (showresults-ROW data)]
                       [(= x 5) (showresults-Global data)]
                       [(= x 6) (showresults-all data)]
                       [else (displayln "The value is not between 1 and 6")])]
      [else (showresults-all data)]))




(define (main)
  (let loop ()
    (define x1 (filter-1 video-game-sales-data))
    (define x2 (filter-2 x1))
    (define x3 (filter-3 x2))
    (define x4 (filter-4 x3))
    (define x5 (sorted x4))
    (filter-5 x5)
    (display "Do you want to exit? (yes/no): ")
    (define answer (read-line))
    (unless (string=? answer "yes")
      (loop))))

(main)
