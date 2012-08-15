(define atom? 
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat? 
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((atom? (car lat)) (lat? (cdr lat)))
     (else #f))))

(define member? 
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (member? a (cdr lat)))))))
;;remove member 
(define remember-bad
  (lambda (a lat)
    (cond 
     ((null? lat) '())
     (else 
      (cond
       ((eq? (car lat) a) (cdr lat))
       (else (remember a
                       (cdr lat))))))))
;;remember using cons to 
;;build a list of atoms that don't match our eq condition
(define remember
  (lambda (a lat)
    (cond 
     ((null? lat) '())
     ((eq? (car lat) a) (cdr lat))
     (else (cons (car lat)
                 (remember a (cdr lat)))))))

(define firsts 
  (lambda (l)
    (cond 
     ((null? l) '())
     (else (cons (car (car l))
                 (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond 
     ((null? lat) '())
     ((eq? (car lat) old) (cons old
                                (cons new (cdr lat))))
     (else (cons (car lat) 
                 (insertR new old
                          (cdr lat)))))))


(define subst
  (lambda (new old lat)
    (cond 
     ((null? lat) '())
     ((eq? (car lat) old)
      (cons new (cdr lat)))
     (else (cons (car lat) 
                 (subst new old 
                        (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond 
     ((null? lat) '())
     ((eq? (car lat) a) 
      ;;ignore it! 
      (multirember a (cdr lat)))
     (else 
      ;;combine current a with the result of a 
      ;;call to mutlirember witht he tail of lat 
      (cons (car lat) 
            (multirember a (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond 
     ((null? lat) '())
     ((eq? (car lat) old)
      ;;we need to prepend NEW to the value 
      ;; of the recur to multisubst. 
      (multisubst new old 
                  (cdr lat)))
     (else 
      ;;otherwise, do the same, but with the existing list 
      ;; atom, and not NEW 
      (cons (car lat) 
            (multisubst new old 
                        (cdr lat)))))))



(define add1
  (lambda (x)
    (+ x 1)))

(define sub1
  (lambda (x)
    (- x 1)))


(define plus`
  ;;this function adds 1 to x, *y* times 
  ;;count down from Y, adding one each time 
  (lambda (x y) 
    (cond 
     ((null? y) x) ;;When y is 0, we just have X left to add
     (else 
      (add1 (plus` x (sub1 y)))))))

(define sub`
  (lambda (x y)
    (cond
     ((null? y) x)
     (else 
      (sub1 (sub` x (sub1 y)))))))

;;adds all numbers in a tuple 
(define addtup
  (lambda (tup)
    (cond 
     ((null? tup) 0) ;;base case (all numbers in list have been exhausted
     (else 
      (+ (car tup)
         (addtup (cdr tup)))))))

(define times 
  (lambda (x y)
    (cond 
     ((null? y) 0)
     (else 
      (+ x (times x (sub1 y)))))))

(define tup+ 
  ;;Function to 
  (lambda (tup1 tup2) 
    (cond 
     ;;((and (null? tup1) (null? tup2)) '())
     ((null? tup1) tup2) ;;Returning tup2/tup1 here will eventually arrive at '() via cdr
     ((null? tup2) tup1)
     (else
      (cons (+ (car tup1) (car tup2))
            (tup+ (cdr tup1) 
                  (cdr tup2)))))))

(define gt
  (lambda (x y)
    (cond 
     ((zero? x) #f)
     ((zero? y) #t)
     (else 
      (gt (sub1 x) (sub1 y))))))

(define lt
  (lambda (x y) 
    ((zero? x) #t)
    ((zero? y) #f)
    (else 
     (lt (sub1 x) (sub1 y)))))

(define pow
  (lambda (x y) 
    ((zero? y) 1)
    (else 
     (* x (pow x (sub1 y))))))

(define len
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else 
      (add1 (len (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond 
     ((zero? n) (car lat))
     (else 
      (cons (car lat) 
            (pick (sub1 n) (cdr lat)))))))

(define rempick
  (lambda (n lat)
    (cond 
     ((zero? (sub1 n)) (cdr lat))
     (else
      (cons (car lat) 
            (rempick (sub1 n)
                     (cdr lat)))))))

(define no-nums 
  (lambda (lat) 
    (cond 
     ((null? lat) '())
     (else 
      (cond
       ((number? (car lat)) ;;skip the number 
        (no-nums (cdr lat)))
       (else 
        (cons (car lat)
              (no-nums (cdr lat)))))))))

(define all-nums 
  (lambda (lat)
    (cond 
     ((null? lat) '())
     (else 
      (cond
       ((number? (car lat))
        (cons (car lat)
              (all-nums (cdr lat))))
       (else ;;otherwise - ignore non-nums 
        (all-nums (cdr lat))))))))

;;(define eqan? 
;;  (lambda (a1 a2) 
;;    (cond 
;;     ((and (number? a1) (number a2))
;;      (= a1 a2))
;;     ((or (

(define occur
  ;;counts occurances of a in lat 
  (lambda (a lat)
    (cond 
     ((null? lat) 0) ;;0 because we are creating a number w/ + not a list!
     (else 
      (cond 
       ((eq? a (car lat))
        (add1 (occur (cdr lat))))
       (else (occur (cdr lat))))))))

(define one?
  (lambda (x) 
    (= x 1)))

;;rewrite of rempick 
;;removes nth atom from a list 
(define rempick'
  (lambda (n lat)
    (cond 
     ((one? n) (cdr lat)) ;;ignores (car lat)
     (else 
      (cons (car lat) 
            (rempick (sub1 n)
                     (cdr lat)))))))


(define rember*
  (lambda (a l) 
    (cond 
     ((null? l) '())
     ;;from here, we have an atom OR a list of S-exps 
     ((atom? (car l))
      (cond 
       ;;if we have an atom...
       ((eq? a (car l)) ;;if a is equal to the atom we found 
        ;;we ignore it of course, continue as normal 
        (rember* a (cdr l)))))
     (else ;; if we found an S-exp, we need to get clever.
      (cons (rember* a (car l))
            (rember* a (cdr l))))))
        

(define insertR* 
  ;;inserts new after old wherever 
  (lambda (new old l)
    ;;adds new after old in list (nested) l
    (cond 
     ((null? l) '())
     ((atom? (car l))
      (cond
       ;;we have an atom, is it equal? 
       ((eq? (car l) old)
        ;;cons the OLD onto the result of (cons new (result of insertR with cdr)
        (cons old
              (cons new 
                    (insertR* new old (cdr l)))))
       (else 
        ;;skip the insertion of NEW, and recur with cdr as normal
        (cons (car l)
              (insertR* new old (cdr l)))))
      (else 
       ;;if we are looking at a non-atom: 
       ;;we have to recur on the CAR and CDR - consing them together
       (cons (insertR* new old (car l))
             (insertR* new old (cdr l))))))))
       

(define occur*
  (lambda (a l) 
    (cond 
     ((null? l) 0)
     ((atom? (car l)
      ;;if (car l) is atomic 
      (cond 
       ((eq? (car l) a)
        ;;we are building a number here, so + is our outer expression 
        (+ 1 (occur* a (cdr l))))
       (else 
        ;;recur normally 
        (occur* a (cdr l))))))
     (else 
      ;;LIST! we have to recur on cdr and car - but we are not building a list... what are we building? 
      (+ (occur* a (car l)) ;; a number it would appear 
         (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond 
     ((null? l) '())
     ((atom? l)
      (cond 
       ((eq? (car l) old)
        ;;replace 
        (cons new
              (subst* new old (cdr l))))
       (else
        ;;don't replace (cons old value with result of a recur)
        (cons (car l)
              (subst* new old (cdr l))))))
     (else 
      ;;recur car and cdr 
      (cons (subst* new old (car l))
            (subst* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond 
     ((null? l) #f)
     ((atom? (car l))
      (or
       ((eq? (car l) a) #t)
       (member* a (cdr l))))
     (else 
      (or
       (member* a (car l))
       (member* a (cdr l)))))))




        
                     
      
     
