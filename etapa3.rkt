#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")
(require "etapa2.rkt")

(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor 
;; arborelui de sufixe:
;; - găsirea unui șablon într-un text
;; - cel mai lung subșir comun a două texte
;; - găsirea unui subșir de lungime dată care se
;;   repetă în text
;; Conform convenției din etapele anterioare, un text
;; este întotdeauna reprezentat ca listă de caractere.
;; Rezultatele funcțiilor de mai jos sunt de asemenea
;; reprezentate ca liste de caractere.


; TODO 1
; Implementați funcția substring? care primește un text și
; un șablon nevid și întoarce true dacă șablonul apare în 
; text, respectiv false în caz contrar.
; Observație: ați implementat deja logica principală a
; acestei căutări în etapa 1, în funcția st-has-pattern?,
; care este un operator al tipului ST. Acum aveți toate
; uneltele necesare implementării operatorului corespunzător
; pentru tipul text (pentru că în etapa 2 ați implementat
; construcția arborelui de sufixe asociat unui text).
(define (substring? text pattern)
  (st-has-pattern? (text->cst text) pattern)
  )


; TODO 2
; Implementați funcția longest-common-substring care primește
; două texte și determină cel mai lung subșir comun al
; acestora, folosind algoritmul următor:
; 1. Construiește arborele de sufixe ST1 pentru primul text.
; 2. Pentru fiecare sufix din al doilea text (de la cel mai
;    lung la cel mai scurt), găsește cea mai lungă potrivire 
;    cu sufixele din primul text, urmând căile relevante în ST1.
; 3. Rezultatul final este cel mai lung rezultat identificat
;    la pasul 2 (în caz de egalitate a lungimii, păstrăm
;    primul șir găsit).
; Folosiți named let pentru a parcurge sufixele.
; Observație: pentru sufixele din al doilea text nu dorim 
; marcajul de final $ pentru a nu crește artificial lungimea 
; șirului comun cu acest caracter.
; Hint: Revizitați funcția match-pattern-with-label (etapa 1).
(define (longest-common-substring text1 text2)
  (let ((st1 (text->cst text1)) (st2-suffixes (get-suffixes text2)))
    (let iter ((sufs st2-suffixes) (res '()))
      (if (null? sufs) res
          (let inner-iter ((mpwl (match-pattern-with-label st1 (car sufs))) (inner-res '()))
            (cond
              ((equal? mpwl #t) (if (> (length (car sufs)) (length res)) (iter (cdr sufs) (car sufs)) (iter (cdr sufs) res)))
              ((list? mpwl) (if (car mpwl)
                                (inner-iter (match-pattern-with-label (caddr mpwl) (cadr mpwl)) (append inner-res (car mpwl)))
                                (if (> (length (append inner-res (cadr mpwl))) (length res)) (iter (cdr sufs) (append inner-res (cadr mpwl))) (iter (cdr sufs) res))))
              (else (iter (cdr sufs) res))
              )
            )
          )
      )
    )
  )

; TODO 3
; Implementați funcția repeated-substring-of-given-length
; care primește un text și un număr natural len și
; parcurge arborele de sufixe al textului până găsește un
; subșir de lungime len care se repetă în text.
; Dacă acest subșir nu există, funcția întoarce false.
; Obs: din felul în care este construit arborele de sufixe
; (pe baza alfabetului sortat), rezultatul va fi primul 
; asemenea subșir din punct de vedere alfabetic.
; Ideea este următoarea: orice cale în arborele de sufixe
; compact care se termină cu un nod intern (un nod care 
; are copii, nu este o frunză) reprezintă un subșir care
; se repetă, pentru că orice asemenea cale reprezintă un
; prefix comun pentru două sau mai multe sufixe ale textului.
; Folosiți interfața definită în fișierul suffix-tree
; atunci când manipulați arborele.
(define (is-leaf? branch)
  (st-empty? (cdr branch)))

(define (repeated-substring-of-given-length text len)
  (let ((result
           (let recursive ((st (text->cst text)) (len len))
             (let ((result
                    (let iter ((st st) (res '()) (aux '()))
                      (let* ((branch (first-branch st))
                             (branch-label (if (null? branch) '() (get-branch-label branch)))
                             (curr-len (length (append aux branch-label))))
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