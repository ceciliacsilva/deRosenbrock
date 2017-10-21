#lang racket

(require "funcoes.rkt")
(require "relatorios.rkt")

(define *output* "output/")

(define (de-relatorio simulName)
  (let ( (file (string-append *output* simulName)) )
    (make-directory* *output*)
    (html-begin file)
    
    (let ( (des (list
                 (make-de 100 -1 2 10 0.9 0.6 200   200)
                 (make-de 100 -1 2 10 0.9 0.6 2000  2000)
                 (make-de 100 -1 2 10 0.9 0.6 10000 10000)
                 (make-de 100 -1 2 10 0.9 0.6 20000 20000))) )
      (for/list ( (de (in-list des)) )
        (html-de-parametros de file)
        (html-interacoes (list (find-min de)) file de)
        )
      )
    (html-end file) 
    ))
