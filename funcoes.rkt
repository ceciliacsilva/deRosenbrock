#lang racket

(define-struct de
  (np xMin xMax n cr f endSimul nRepeat))

(define *de*
  (make-de 100 -1 2 10 0.9 0.6 200 200))

(define rosenbrock
  (lambda(xs)
    (let loop ( (xsl xs) (result 0))
      (match xsl
        ( (list xi xi+1 rest ...)
          (loop (cons xi+1 rest) (+ (+ (* 100 (sqr (- xi+1 (sqr xi)))) (sqr (- 1 xi))) result))
          )
        (_ result))
      )
    ))

(define (populacao-inicial de)
  (let ( (popSize (de-np de))
         (n (de-n de)) )
    (for/list ( (i (in-range popSize)) )
      (let ( (c (cromossomo-criar n)) )
        (cons c (fitness-eval c de))
        ))
    ))

(define (find-min de)
  (let ( (popSize       (de-np de))
         (maxInteracoes (de-endSimul de))
         (maxRepetido   (de-nRepeat de)) )
    (let* ( (popInicial    (populacao-inicial de)) )
      (let loop ( (pop0   popInicial)
                  (best   '())
                  (repeatBest  0)
                  (repeat      1) )
        (let ( (newPop (operacoesGeneticas pop0 de))  )
          (let* ( (pop1 (populacao-elite newPop popSize))
                  (bestPop (first (populacao-elite pop1 1))) )
            (if (or (= repeat maxInteracoes) (= repeatBest maxRepetido))
                (begin
                  best)
                (loop pop1
                      bestPop
                      (if (equal? best bestPop) (add1 repeatBest) 0)
                      (add1 repeat) ))
            )
          )
        ))
    ))

(define (best-interacao interacao)
  (cdr (car (sort interacao #:key cdr <))))

(define (populacao-elite pop n)
  (let ( (popOrdem (sort pop #:key cdr <)) )
    (take popOrdem n)) )

(define (desnormalize xs de)
  (let ( (xMin (de-xMin de))
         (xMax (de-xMax de)) )
    (map (lambda(x)
           (+ (* x (- xMax xMin)) xMin)) xs)) )

(define (cromossomo-criar n)
  (for/list ( (i (in-range n)) )
    (random)
    )
  )

(define (fitness-eval cromossomo de)
  (let ( (f rosenbrock) )
    (let ( (xsDesnomalizados (desnormalize cromossomo de)) )
      (f xsDesnomalizados)
      ))
  )

(define (targets popTotal)
  (let ( (targetsDisponiveis popTotal) )
    (lambda ()
      (if (null? targetsDisponiveis) #f
          (let* ( (targetPos (random (length targetsDisponiveis)))
                  (target (list-ref targetsDisponiveis targetPos)) )
            (set! targetsDisponiveis (remove target targetsDisponiveis))
            target)
          )
      )
    ))

(define (operacoesGeneticas popTotal de)
  (let ( (targetAtual (targets popTotal)) )
    (let loop ( (newPop '()) )
      (let* ( (xiG (targetAtual))
              (popSemTarget (remove xiG popTotal)) )
        (if xiG
            (let ( (viG (mutacao-diferencial xiG popSemTarget de))
                   (xiGFitness (cdr xiG)) )
              (let* ( (uiG (crossover viG (car xiG) de))
                      (uiGFitness (fitness-eval uiG de)) )
                (if (< xiGFitness uiGFitness)
                    (loop (cons xiG newPop))
                    (loop (cons (cons uiG uiGFitness) newPop))
                    )
                )
              )
            newPop)
        )
      )
    ))

(define (mutacao-diferencial xiG pop de)
  (let ( (F (de-f de)) )
    (let ( (xr2G (car (random-elemento pop)))
           (xr3G (car (random-elemento pop))) )
      (let ( (diferencaF (map (lambda(a b) (* F (- b a))) xr3G xr2G)) )
        (map (lambda (a b) (+ a b)) (car xiG) diferencaF)
        )
      ))
  )

(define filter0-1 (lambda(x) (and (<= x 1) (>= x 0) x)))

(define (crossover viG xiG de)
  (let ( (cr (de-cr de))
         (li (random (length viG))) )
    (let ( (newIndividuo
            (for/list ( (vi (in-list viG))
                        (xi (in-list xiG))
                        (j  (in-naturals)) )
              (let ( (rj (random)) )
                (cond ( (and
                         (or (= li j)
                             (<= rj cr))
                         (filter0-1 vi)) vi)
                      (else xi)) )  ))    )
      newIndividuo)
    ))

(define (random-elemento pop)
  (let ( (pos (random (length pop))) )
    (list-ref pop pos)))