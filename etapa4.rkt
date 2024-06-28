#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")
(require "datadump.rkt")
(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

(define (tail-longest-common-prefix w1 w2 rez)
  (cond
    ((or (null? w1) (null? w2)) (list rez w1 w2))
    ((equal? (car w1) (car w2))
     (tail-longest-common-prefix (cdr w1) (cdr w2) (append rez (list (car w1))))
     )
    (else (list rez w1 w2))
    ))

; list list -> list
(define (longest-common-prefix w1 w2)
  (tail-longest-common-prefix w1 w2 '())
  )

; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection

; stream -> list (string)
(define (longest-common-prefix-of-collection words)
  (let loop ((prefix (collection-car words))
             (words (collection-cdr words)))
    (if (collection-null? words)
        prefix
        (loop (car (longest-common-prefix prefix (collection-car words)))
              (collection-cdr words)))))

; stream list(string) -> list(list, list, stream) / list(#f list)
(define (match-pattern-with-label st pattern)
  (let ((branch (get-ch-branch st (car pattern))))
    (if (pair? branch)
        (let* ((branch-label (get-branch-label branch))
               (lcp (longest-common-prefix branch-label pattern))
               (carlcp (collection-car lcp)))
          (cond
            ((equal? carlcp pattern) #t)
            ((equal? carlcp branch-label)
             (list branch-label
                   (collection-caddr lcp)
                   (get-branch-subtree branch)
                   ))
            (else (list #f carlcp))))
        (list #f '()))))

; stream, list -> boolean
(define (st-has-pattern? st pattern)
  (let ((output (match-pattern-with-label st pattern)))
    (cond
      ((list? output)
       (if (list? (car output))
           (st-has-pattern? (caddr output) (cadr output))
           #f))
      (else #t))
    )
  )

; list -> stream
(define (get-suffixes text)
  (if (null? text) (null-collection)
      (collection-cons text (get-suffixes (cdr text)))))

; stream, char -> stream
(define (get-ch-words words ch)
  (collection-filter
   (lambda (word)
       (and (not (null? word)) (equal? ch (car word))))
   words))

; stream -> pair
(define (ast-func suffixes)
  (cons
   (list (car (collection-car suffixes)))
   (collection-map
    (lambda (word)
      (cdr word))
    suffixes
   )))

(define (cst-func suffixes)
  (let ((prefix (longest-common-prefix-of-collection suffixes)))
    (cons
     prefix
     (collection-map
      (lambda (word)
        (drop word (length prefix)))
      suffixes
      ))))

; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)
(define (suffixes->st labeling-func suffixes alphabet)
  (if (collection-null? suffixes) (null-collection)
  (collection-filter
   (lambda (elem) (not (null? elem)))
   (collection-map
    (lambda (letter)
      (let* ((filtered-suffixes
              (collection-filter
               (lambda (suffix)
                 (equal? letter (car suffix)))
               suffixes
               ))
             (labeling-func-res
              (if (collection-null? filtered-suffixes) '()
                  (labeling-func filtered-suffixes))
              ))
        (if (null? labeling-func-res) '()
            (cons
             (car labeling-func-res)
             (if (null? (collection-car (cdr labeling-func-res))) (null-collection)
                 (suffixes->st labeling-func (cdr labeling-func-res) alphabet)
                 )
             )
            )
        )
      )
    alphabet
    )
  )
  )
  )


; nu uitați să convertiți alfabetul într-un flux
(define text->st
  (lambda (text)
    (lambda (labeling-func)
      (let* ((text-with-$ (append text '(#\$)))
             (suffixes (get-suffixes text-with-$))
             (alphabet (sort (remove-duplicates text-with-$) char<?)))
      (suffixes->st labeling-func suffixes (list->collection alphabet))
    ))))


(define text->ast
  (lambda (text)
    ((text->st text) ast-func)))


(define text->cst
  (lambda (text)
    ((text->st text) cst-func)))

; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  (st-has-pattern? (text->ast text) pattern)
  )

; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (is-leaf? branch)
  (st-empty? (cdr branch)))

(define (repeated-substring-of-given-length text len)
  (let ((result
           (let recursive ((st (text->cst text)) (len len))
             (let ((result
                    (let iter ((st st) (res '()) (aux '()))
                      (let* ((branch (if (collection? (first-branch st)) (collection->list (first-branch st)) (first-branch st)))
                             (branch-label (if (null? branch) '() (get-branch-label branch)))
                             (curr-len (+ (length aux) (length branch-label))))
                        (cond
                          ((not (null? res)) (append aux res))
                          ((null? branch) '())
                          ((is-leaf? branch) (iter (other-branches st) res aux))
                          ((< curr-len len)
                           (let ((part-res
                                  (recursive
                                   (get-branch-subtree branch)
                                   (- len curr-len)
                                   )))
                             (iter
                              (other-branches st)
                              part-res
                              (if (null? part-res) '() branch-label)
                              )
                             )
                           )
                          (else (take (append branch-label res) len))
                          )
                        )
                      )
                    ))
               (cond
                 ((st-empty? st) '())
                 (else result)
                 )
               )
             )))
    (if (null? result) #f result)
    )
  )
