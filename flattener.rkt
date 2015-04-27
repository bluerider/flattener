#lang racket

;; load the following modules
(require compiler/cm)
(require racket/cmdline)
(require racket/file)

(provide string-split-parens)
(provide strip-quotes)
(provide non-built-in-module?)
(provide file->module)
(provide get-required-modules)
(provide get-all-required-modules)
(provide return-sorted-modules)
(provide flatten-modules)
(provide string-custom-append*)
(provide flatten-require)
(provide strip-comments)
(provide flatten-top-level-require)

;; split strings by parenthesis (return parentheses as separator)
(define (string-split-parens string)
  (define (iterator list-of-chars split-list tmp-list) 
    (if (null? list-of-chars)
        (map list->string (reverse (cons tmp-list split-list)))
        (let ([char (car list-of-chars)])
             (if (or (equal? #\) char)
                     (equal? #\( char))
                 (iterator (cdr list-of-chars)
                           (cons (list char)
                             (if (null? tmp-list)
                                 split-list
                                 (cons (reverse tmp-list)
                                       split-list)))
                           '())
                 (iterator (cdr list-of-chars)
                           split-list
                           (cons char tmp-list))))))
  (iterator (string->list string) '() '()))
  
;; determine if a file isn't a build in module
(define (non-built-in-module? string)
  (ormap (lambda (split-string)
            (string=? "rkt" split-string))
         (string-split string ".")))
         
;; strip quotes from a string
(define (strip-quotes string)
  (car
   (filter-not (lambda (split-string)
                 (string=? "" split-string))
              (string-split string "\""))))

;; flatten the required modules
(define (flatten-require string)
  (define (iterator chars new-chars boolean)
    (cond ((null? chars)
           (list->string (reverse new-chars)))
          ((and (equal? #\" (car chars))
                (false? boolean))
           (iterator (cdr chars)
                     (append (reverse (string->list "(submod \"..\" "))
                           new-chars)
                     #t))
          ((and (equal? #\" (car chars))
                (not (false? boolean)))
           (iterator (cdr chars)
                     (cons #\) new-chars)
                     #f))
          (else
           (iterator (cdr chars)
                     (cons (car chars) new-chars)
                     boolean))))
  (iterator (string->list string) '() #f))
  
;; flatten top-level module
(define (flatten-top-level-require string)
  (define (iterator chars new-chars boolean)
    (cond ((null? chars)
           (list->string (reverse new-chars)))
          ((and (equal? #\" (car chars))
                (false? boolean))
           (iterator (cdr chars)
                     (cons #\' new-chars)
                     #t))
          ((and (equal? #\" (car chars))
                (not (false? boolean)))
           (iterator (cdr chars)
                     new-chars
                     #f))
          (else
           (iterator (cdr chars)
                     (cons (car chars) new-chars)
                     boolean))))
  (iterator (string->list string) '() #f))

;; strip comments from file
;;   return a a list of strings
(define (strip-comments file)
  (define (remove-comments chars okay-chars)
    (if (or (null? chars)
            (equal? #\; (car chars)))
        (list->string (reverse okay-chars))
        (remove-comments (cdr chars)
                         (cons (car chars)
                         okay-chars))))
  (define (remove-comments-line line)
    (remove-comments (string->list line) '()))
  (string-append* (map remove-comments-line (file->lines file))))

;; append strings together
(define (string-custom-append* . lat)
  (list->string
    (append-map
      (lambda (string)
        (string->list string))
      lat)))
         
;; convert requires to use ""->'
;; produce a module

;; return the arguments to a process
(define (return-require-args file)
    (foldl (lambda (string condensor)
             (let ([split-string (string-split string)])
                  (cond ((null? split-string)
                          condensor)
                        ((string=? "require"
                                   (car split-string))
                         (cons (strip-quotes (car (string-split string "require ")))
                               condensor))
                        (else
                          condensor))))
           '()
           (string-split-parens (file->string file))
           ))
            
;; get the required modules of a file
(define (get-required-modules file)
  (return-require-args file))
      
;; recursively get all required modules of a file
(define (get-all-required-modules file)
  (let ([found-modules (get-required-modules file)])
           (append found-modules 
                 (append-map get-all-required-modules
                   (filter non-built-in-module? found-modules)))))
                           
;; return a sorted list of modules according to the # of times they are required
(define (return-sorted-modules modules)
  ;; generate a list of needed modules and their frequencies of use
  ;;   (int string)
  (define (iterator modules sorted-modules)
    (if (null? modules)
        sorted-modules
        (let ([filtered-modules (remove* (list (car modules))
                                         modules)])
             (iterator filtered-modules
                       (cons (list (- (length modules) (length filtered-modules))
                                   (car modules))
                             sorted-modules)))))
 (map cadr
  ;; sort modules by frequency usage
  (sort (iterator modules '()) (lambda (first second)
                                 (>= (car first)
                                     (car second))))))
  
;; need to solve #lang issues; can be converted to (module s-exp <#lang>)
(define file->module
  (lambda (file [top-level? #f])
    (string-append*
      (reverse
        (foldl (lambda (string condensor)
                  (let ([split-string (string-split string)])
                       (cond ((null? split-string)
                              condensor)
                             ((and (false? top-level?)
                                   (string=? "require" (car split-string)))
                              (cons
                                (string-append*
                                   "require "
                                   (map flatten-require (cdr split-string)))
                                 condensor))
                             ((and (not (false? top-level?))
                                   (string=? "require" (car split-string)))
                              (cons
                                (string-append*
                                   "require "
                                   (map flatten-top-level-require (cdr split-string)))
                                 condensor))
                             ((and (string=? "#lang" (car split-string))
                                   (false? top-level?))
                              (cons (string-custom-append* 
                                               "(module "
                                               (strip-quotes file)
                                               " "
                                               (cadr split-string)
                                               (list->string (list #\newline)))
                                    condensor))
                             ((and (string=? "#lang" (car split-string))
                                   (not (false? top-level?)))
                              condensor)
                             (else
                              (cons string condensor)))))
               '()
               (append (string-split-parens (strip-comments file))
                      (if (false? top-level?)
                          (list ")")
                          '())))))))
                          
;; get the #lang of a rkt file
(define (get-lang file)
  (car (file->lines file)))
  
;; put all built in racket modules in the car of the list
(define (flatten-modules file)
  (let ([mega-file (string-append (car (string-split file "."))
                                   "-mega.rkt")])
       (with-output-to-file mega-file
         #:exists 'replace
         (lambda ()
          (displayln (get-lang file))
          (for-each 
             (lambda (module)
                (if (non-built-in-module? module)
                    (displayln (file->module module))
                    #f))
             (return-sorted-modules (get-all-required-modules file)))
          (displayln (file->module file #t))))
       mega-file))
       
;; run with cmdline argument if called
(module* main #f
  (flatten-modules (car (vector->list (current-command-line-arguments)))))