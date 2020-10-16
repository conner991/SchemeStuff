;   HW 2, CS 326
;   Conner Fissell

; HELPER FUNCTIONS
; Function to check if input is an atom
    (define (atom? x)
        (and (not (pair? x)) (not (null? x))))


; Fuction to check for duplicates in a list 'lis'
    (define (any-dup? num lis)
        (if (null? lis) #f 
            (if (= num (car lis)) #t
                (any-dup? num (cdr lis)))))



; 1.) Consider an implementaion of sets with scheme lists, a set is an unordered collection 
;     of elements, without duplicates. 
;   
;   a. Write a recursive function (is-set? L), which determines whether the list L is a 
;       set. 

    ; is-set? uses any-dup? to determine whether the list L is a set.
    (define (is-set? L)
        (cond 
            ((null? L) #t)              ; Return #t if the list is empty 
            ((any-dup? (car L) (cdr L)) ; Return #f if the first element in the list mathches any others
                #f)
            (else (is-set? (cdr L)))))  ; else repeat with the list minus the first element 


;   b. Write a recursive function (make-set L), which returns a set built from list L. 
;       by removing duplicates, if any. Remember that the order of set elements does not matter.

    ; make-set? uses any-dup? to find duplicates in the set. 
    (define (make-set L)
        (cond
            ((null? L) L)                               ; Is the set an empty set? 
            ((any-dup? (car L) (cdr L))                 ; Check for duplicates
                (make-set (cdr L)))                     ; Call make-set again with less of the set if duplicate found
            (else (cons (car L) (make-set (cdr L))))))  ; Else append the first element to the rest of the set using recursion 


;   c. Write a recursive function (subset? A S), which determines whether the set A is a subset of 
;       the set S.

    (define (subset? A S)
        (cond
            ((null? A)                  ; Return #t if the first set is empty 
                #t)
            ((any-dup? (car A) S)       ; If the first element in A repeats in S, then check for more in A
                (subset? (cdr A) S))
            (else #f)))                 ; Else return false


;   d. Write a recursive function (union A B), which returns the union of sets A and B.

    (define (union A B)
        (cond
            ((null? A)                                  ; If set A or B is empty, return the other
                B)
            ((null? B)
                A)
            ((any-dup? (car A) B)                       ; Check to see if the first element in A is in B at all
                (union (cdr A) B))                      ; If so then repeat the function with the duplicate missing
            (else (cons (car A) (union (cdr A) B)))))   


;   e. Write a recursive function (intersection A B), which returns the intersection of sets 
;       A and B.

    (define (intersection A B)
        (cond
            ((null? A)                                     ; If A is an empty set, then return an empty set
                `())                                        
            ((any-dup? (car A) B)                          ; Check to see if the first element is in B at all
                (cons (car A) (intersection (cdr A) B)))   ; If so then then save that as a head to be attached later
            (else (intersection (cdr A) B))))



; 2.) Consider an implementation of binary trees with Scheme lists

    (define T   
        '(13  
            (5     
                (1 () ())     
                (8 ()        
                    (9 ()())))  
            (22  
            (17 () ())  
            (25 () ()))))

; Before proceeding, it  may  be  useful  to  define  three  auxiliary  functions 
; (root-val  T), (left T) and (right T), which return the value in 
; the root of tree T, its left subtree and its right subtree, respectively. 

; HELPER FUNCTIONS 
    (define (root-val T)
        (car T))
    (define (left T) 
        (cadr T))
    (define (right T) 
        (caddr T))

;   a. Write a recursive function (tree-member? V T), which determines whether V appears as an
;       element in the tree T. 

    ;(define (tree-member? V T)
    ;    (cond
    ;        ((null? T)                                                          ; If T is empty then return false
    ;            #f)
    ;        ((list? (root-val T))                                               ; Check to see if the root is a list
    ;            (or (tree-member? V (root-val T)) (tree-member? V (cdr T))))    ; If so, continue searching through the branches 
    ;        ((equal? V (root-val T))                                            ; until the value is found or not.
    ;            #t)
    ;        (else (tree-member? V (cdr T)))))

    ;;;;;;;;;; REVIEW VERSION;;;;;;;;;;;;;;
    (define (tree-member? V T)
        (cond 
            ((null? T)                  #f)
            ((equal? V (root-val T))    #t)
            (else (or (tree-member? V (left T)))  (tree-member? V (right T)))))


;   b. Write a recursive function (preorder T), which returns the list of all elements in the tree
;       T corresponding to a preorder traversal of the tree. 

    ;(define (preorder T)
    ;    (cond
    ;        ((null? T)
    ;            `())
    ;        ((and (null? (left T)) (null? (right T)))                           ; If the left and right lists are null, then return the root value
    ;            (list (root-val T)))
    ;        ((and (not (null? (left T))) (null? (right T)))                     ; If the left list is not empty and the right one is, append the root value
    ;            (append (list (root-val T)) (preorder (left T))))               ; to the result of a preorder left traversal.
    ;        ((and (null? (left T)) (not (null? (right T))))                     ; If the right list is not empty and the left one is, append the root value 
    ;            (append (list (root-val T)) (preorder (right T))))              ; to the result of a preorder right traversal. 
    ;        (else (append (list (root-val T)) (append (preorder (left T)) (preorder (right T)))))))

    
    ;;;;;;;;; REVIEW VERSION ;;;;;;;;;;;;;
    (define (preorder T)
        (cond
            ((null? T)  `())
            (else   (cons (root-val T) (append (preorder (left T))  (preorder (right T)))))))


;   c. Write a recursive function (inorder T), which returns the list of all elements in the tree T
;       corresponding to an inorder traversal of the tree. 

    ;(define (inorder T)
    ;    (cond 
    ;        ((null? T)
    ;            `())
    ;        ((and (null? (left T)) (null? (right T)))
    ;            (list (root-val T)))
    ;        ((and (not (null? (left T))) (null? (right T)))
    ;            (append (inorder (left T)) (list (root-val T))))
    ;        ((and (null? (left T)) (not (null? (right T))))
    ;            (append (list (root-val T)) (inorder (right T))))
    ;        (else (append (inorder (left T)) (list (root-val T)) (inorder (right T))))))

    ;;;;;;;;; REVIEW VERSION ;;;;;;;;;;;;;
    (define (inorder T)
        (cond
            ((null? T)  `())
            (else   (append (inorder (left T)) (list (root-val T)) (inorder (right T))))))

; 3.) Write a recursive function (deep-delete V L), which takes as arguments a value V and a list 
;   L, and returns a list identical to L except that all occurances of V in L or in any sublist of L
;   have been deleted. 

    ;;;;;;;;; REVIEW VERSION ;;;;;;;;;;;;;
    (define (deep-delete V L)
        (cond 
            ((null? L)  `())
            ((list? (car L))        (cons (deep-delete V (car L)) (deep-delete V (cdr L))))
            ((equal? V (car L))     (deep-delete V (cdr L)))
            (else                   (cons (car L) (deep-delete V (cdr L))))))


; 4.) A binary search tree is a binary tree for which the value in each node is greater than or equal
;   to all values in its left subtree, and less than all values in it's right subtree. The binary tree
;   given as an example in problem 2 also qualifies as a binary search tree. Using the same list 
;   representation, write a recursive function (inser-bst V T), which returns the binary search tree
;   that results by inserting value V into binary search tree T. 

    (define (insert-bst V T)
        (cond
            ((null? T)            (list V `() `())
            ((<= V (root-val T))  (list (root-val T)  
                                        (insert-bst V (left T))  
                                        (right T)))
            (else                 (list (root-val T)  
                                        (left T)                
                                        (insert-bst V (right T)))))))















(define num1 3)
(define num2 2)
(define num3 1)

(define (add-nums num1 num2)
	(+ num1 num2))

(define (test)
	(define num1 4)
	(display (list "the first number is: " num1))
	(newline)
	(display (list "the second number is: " num2))
	(newline)
	(display (list "their sum is: " (add-nums num1 num2))))


;;;;;;;;;;;;;;;;;; HW 4
; Helper Function
(define (adder list x)
	(cond
		((null? (cdr list)) (+ (car list) x))         ; Base case
		(else (adder (cdr list) (+ (car list) x)))))   ; Tail Resursive Part

; Main Summation Function
(define (addList list)
	(adder list 0))

