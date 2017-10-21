#lang racket

(provide (all-defined-out))

(require "funcoes.rkt")

(define (html-begin file)
  (call-with-output-file file #:exists 'replace
    (lambda (p)
      (displayln "<!DOCTYPE html>
<html>
<head>" p)
      (displayln "
<style>
table {
    width:100%;
}
table, th, td {
    border: 1px solid black;
    border-collapse: collapse;
}
th, td {
    padding: 5px;
    text-align: left;
}
table#t01{
    background-color: #fff;
}
tr#best{
   background-color:#462;
}
table#t01 th {
    background-color: #111;
    color: white;
}
</style>
</head>
<body>" p)  ))   )

(define (html-de-parametros de-p file)
  (call-with-output-file file #:exists 'append
    (lambda (p)
       
      (match de-p
        [(de np xMin xMax n cr f endSimul nRepeat) 

         (displayln "<table>
  <tr>
    <th>NP</th>
    <th>xMin</th>
    <th>xMax</th>
    <th>n</th>
    <th>CR</th>
    <th>F</th>
    <th>endSimul</th>
    <th>nRepeat</th>
  </tr>" p)
         
         (displayln "<tr>" p)
         
         (displayln (~a "<td>" np "</td>") p)
         (displayln (~a "<td>" xMin "</td>") p)
         (displayln (~a "<td>" xMax "</td>") p)
         (displayln (~a "<td>" n "</td>") p)
         (displayln (~a "<td>" cr "</td>") p)
         (displayln (~a "<td>" f "</td>") p)
         (displayln (~a "<td>" endSimul "</td>") p)
         (displayln (~a "<td>" nRepeat "</td>") p)
         
         (displayln "</tr>" p)
         (displayln "</table>" p)])  ))   )


(define (html-interacoes interacao file de)
  (call-with-output-file file #:exists 'append
    (lambda (p)
       (let ( (individuo (car interacao)) )
         (displayln "<table id=\"t01\"><tr>" p)
         (for ( (xi (in-list (car individuo)))
                (i (in-naturals)) )
           (displayln (~a "<th>x" i "</th>") p))
         (displayln (~a "<th>Resultado</th>") p)
         (displayln "</tr>" p) )

      (for ( (individuo (in-list interacao)) )
        (let ( (individuo-real (desnormalize (car individuo) de)) )
          (displayln "<tr>" p)
          (for ( (xi (in-list individuo-real)) ) 
            (displayln (~a "<td>" (~r xi #:precision 10) "</td>") p))
          (displayln (~a "<td>" (~r (cdr individuo) #:precision 10)"</td>") p)
          (displayln "</tr>" p) ) )
      
      (displayln "</table><br></br><hr><hr><br></br>" p)
      ))  )

(define (html-end file)
  (call-with-output-file file #:exists 'append
    (lambda (p)
      (displayln "</body>
</html>" p)) ))