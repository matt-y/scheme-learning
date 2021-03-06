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


(define leftmost 
  (lambda (l)
    (cond 
     ;;base case: we found an atom 
     ((atom? (car l))
      (car l))
     (else 
      (leftmost (car l))))))

;(define eqlist
;  (lambda (l1 l2)
;    (cond 
;     ;;checks if one, the other, or both ARRRR empty
;     ((and (null? l1) (null? l2)) #f)
;     ((or (null? l1) (null? l2)) #f)
;     (
      
(define numbered? 
  (lambda (aexp)
    (cond 
     ((atom? aexp) (number? aexp))
     ((eq? (car (cdr aexp)) '+)
      (and (numbered? (car aexp))
           (numbered? (car (cdr (cdr aexp)))))
     ((eq? (car (cdr aexp)) 'x)
      (and (numbered? (car aexp))
           (numbered? (car (cdr (cdr aexp)))))
     ((eq? (car (cdr aexp)) '^)
      (and (numbered? (car aexp))
           (numbered? (car (cdr (cdr aexp)))))))))))

(define numbered?'
  (lambda (aexp)
    (cond 
     ((atom? aexp) (number? aexp))
     (else 
      (and (numbered? (car aexp))
           (numbered? 
            (car (cdr (cdr aexp)))))))))

(define value 
  (lambda (nexp)
    (cond 
     ((atom? nexp) nexp)
     ((eq? (car (cdr nexp)) '+) 
      ;;7th commandment. recur on subparts 
      (+ (value (car nexp))
         (value (cdr (cdr nexp)))))
     ((eq? (car (cdr nexp)) 'x)
      (x (value (car nexp))
         (value (cdr (cdr nexp)))))
     (else 
      (* (value (car nexp))
         (value (cdr (cdr nexp))))))))

(define value'
  ;;this is wrong 
  (lambda (nexp)
    (cond 
     ((atom? nexp) nexp) ;; base case 
     ((eq? (car nexp) '+)
      (+ (value' (cdr nexp))
         (value' (cdr (cdr nexp)))))
     ;;etc
     (else
      (* (value' (cdr nexp))
         (value' (cdr (cdr nexp))))))))


;;extract sub expressions from the form:
;;(+ x y) -> x is first, y is second
(define 1st-sub-xpr
  (lambda (exp)
    (car (cdr exp))))
(define 2nd-sub-xpr
  (lambda (exp)
    (car (cdr (cdr exp)))))
;;extract the operator of (+ x y)
(define operator 
  (lambda (exp)
    (car (exp))))

;;reqrite of value with the above 
(define value 
  (lambda (aexp)
    (cond 
     ((atom? aexp) aexp)
     ((eq? (operator aexp) '+)
      (+ (value (1st-sub-xpr))
         (value (2nd-sub-xpr))))
     ((eq? (operator aexp) 'x)
      (* (value (1st-sub-xpr))
         (value (2nd-sub-xpr))))
     (else 
      (/ (value (1st-sub-xpr))
         (value (2nd-sub-xpr)))))))

;;below is a funky number representation
;;using a list of empty lists 
(define sero
  (lambda (n)
    (null? n)))

;;+ 1 (or in our weird case the empty list)
(define edd1
  (lambda (n)
    (cons '() n)))

(define sub1
  (lambda (n)
    (cdr n)))

(define plus'
  (lambda (n m)
    (cond 
     ((sero m) n)
     (else (edd1 (plus' n (sub1 m)))))))
     



(define set?
  (lambda (l)
    (cond 
     ((null? l) #t)
     ((member? (car l) (cdr l))
      #t)
     (else 
      (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond 
     ((null? lat) '()) 
     ((member? (car lat) (cdr lat))
      (makeset (cdr lat)))
     (else 
      (cons (car l) (makeset (cdr lat)))))))
                     

(define makeset 
  (lambda (lat)
    (cond 
     ((null? lat) '())
     (else
      (cons (car lat)
            (makeset
             (multirember (car lat)
                          (cdr lat))))))))

(define subset
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else 
      (cond 
       ((member? (car set1) set2)
        (subset (cdr set1) set2))
       (else #f))))))

(define intersect? 
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     ((member? (car set1) set2)
      #t)
     (else 
      (intersect? (cdr set1) set2)))))
      
(define intersect
  (lambda (set1 set2)
    (cond 
     ((null? set1) '())
     ((member? (car set1) set2)
      (cons (car set1) 
            (intersect (cdr set1) set2)))
     (else 
      (intersect (cdr set1) set2)))))


(define union
  (lambda (set1 set2)
    (cond 
     ((null? set1) set2)
     (else 
      (cond 
       ;;union still must not be a multiset
       ;;if something is equal skip it 
       ((member? (car set1) set2)
        (union (cdr set1) (set2)))
       (else 
        (cons (car set1) 
              (union? (cdr set1) set2))))))))

(define intersectl
  (lambda (setl)
    (cond 
     ((null? (cdr  setl)) (car setl))
     (else 
      (cons (intersect (car setl)
                       (intersectl (cdr setl))))))))


(define first
  (lambda (p)
    (car p)))
(define second
  (lambda (p)
    (car (cdr p))))
(define third
  (lambda (p)
    (car (cdr (cdr p)))))
(define build 
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))
(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))



;;lambda the ultimate 

(define rember-f
  (lambda (fun a l)
    (cond 
     ((null? l) '())
     ((fun a (car l)) (cdr l))
     (else 
      (cons (rember-f fun 
                      a
                      (cdr l))
            car l)))))

(define eq?-c
  ;;returns a function that takes an argument
  (lambda (a)
    (lambda (x)
      (eq? x a))))


        
(define seql
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

;;function that returns a function using the seq function for
;;insertions 
(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond 
       ((null? l) '())
       
       ((eq? old (car l))
        (seq new old (cdr l)))
       (else 
        (cons (car l)
              ((insert-g seq) new old 
               (cdr l))))))))
