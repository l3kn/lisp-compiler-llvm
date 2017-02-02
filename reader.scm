(def cur-char #f)

(defn read-char ()
      (if (char? cur-char)
        (let ((old cur-char))
          (set! cur-char #f)
          old)
        (getchar)))

(defn peek-char ()
      (if (char? cur-char)
        cur-char
        (let ((new (getchar)))
          (set! cur-char new)
          new)))

(defn skip-chars (n)
      (if (eq? n 0)
          'done
          (begin
            (read-char)
            (skip-chars (fxsub1 n)))))

(defn skip-line ()
      (let ((next-char (peek-char)))
        (if (eq? next-char #\newline)
            ; TODO: Refactor once one-armed ifs are supported
            'done
            (begin
              (read-char)
              (skip-line)))))

(defn read-token ()
      (let ((first-char (read-char)))
        (cond
          ; ((null? first-char)
          ;  'eof)
          ((char-whitespace? first-char)
           (read-token))
          ((eq? first-char #\;)
           (skip-line)
           (read-token))
          ((eq? first-char #\()
           'left-paren)
          ((eq? first-char #\))
           'right-paren)
          ((eq? first-char #\')
           'quote-token)
          ((eq? first-char #\`)
           'quasiquote-token)
          ((eq? first-char #\,)
           'unquote-token)
          ((eq? first-char #\")
           (read-string ""))
          ; TODO: support unquote splicing
          ((eq? first-char #\#)
           (let ((next-char (read-char)))
             (cond
               ((eq? next-char #\\) (read-character))
               ((eq? next-char #\t) #t)
               ((eq? next-char #\f) #f)
               ((eq? next-char #\b)
                (read-number (read-char) 2))
               ((eq? next-char #\o)
                (read-number (read-char) 8))
               ; TODO: Handle hex numbers,
               ; bin & oct work right now bc/
               ; char - 48 = value,
               ; for "A".."F" this is not the case
               (else (error "Illegal symbol after #: " next-char)))))
          ((and (eq? first-char #\-)
                (char-numeric? (peek-char)))
           (fxneg (read-number (read-char) 10)))
          ((char-numeric? first-char)
           (read-number first-char 10))
          ((or (char-alphabetic? first-char)
               (char-special? first-char))
           (list 'escaped (read-identifier first-char)))
          (else
            (error "Illegal lexical syntax: " first-char)))))

(defn escaped? (expr)
      (tagged-list? expr 'escaped))

(def string-escape-codes
     (list (cons #\n #\newline)
           (cons #\t #\tab)
           (cons #\" #\")
           (cons #\\ #\\)))

(defn read-string (acc)
      (let ((first-char (read-char)))
        (cond
          ((eq? first-char #\\)
           (read-string
             (string-append acc
                            (~> (read-char) 
                                (assoc string-escape-codes)
                                rst
                                char->string))))
          ((eq? first-char #\")
           acc)
          (else (~>> first-char
                     char->string
                     (string-append acc)
                     read-string)))))

(defn read-character ()
      (let ((first-char (read-char))
            (next-char (peek-char)))
        (cond
          ((and (eq? first-char #\n)
                (eq? next-char #\e))
           (skip-chars 6)
           #\newline)
          ((and (eq? first-char #\t)
                (eq? next-char #\a))
           (skip-chars 2)
           #\tab)
          ((and (eq? first-char #\s)
                (eq? next-char #\p))
           (skip-chars 4)
           #\space)
          ((and (eq? first-char #\r)
                (eq? next-char #\e))
           (skip-chars 5)
           #\return)
          (else first-char))))

(defn read-identifier (first-char)
      (~> first-char
          (cons '())
          read-identifier_
          list->string
          string->symbol))

(defn read-identifier_ (acc)
      (let ((next-char (peek-char)))
        (if (or (char-alphabetic? next-char)
                (char-numeric? next-char)
                (char-special? next-char))
          (read-identifier_ (cons (read-char) acc))
          (reverse acc))))

(defn read-number (first-char base)
      (~> first-char
          char->fixnum
          (fx- 48)
          (read-number_ base)))

(defn read-number_ (acc base)
      (let ((next-char (peek-char)))
        (if (char-numeric? next-char)
          (read-number_
            (fx+ (fx* acc base)
                 (~> (read-char) char->fixnum (fx- 48)))
            base)
          acc)))

(defn read-all ()
      (read-all_ '()))

(defn read-all_ (acc)
      (let ((next-char (peek-char)))
        (cond
          ((null? next-char)
           (reverse acc))
          ; This is needed if a newline is the last char of a file
          ((char-whitespace? next-char)
           (read-char)
           (read-all_ acc))
          (else
            (read-all_ (cons (read_) acc))))))

(defn read_ ()
      (let ((next-token (read-token)))
        (cond
          ((eq? next-token 'left-paren)
          (read-list '()))
          ((eq? next-token 'quote-token)
           (list 'quote (read_)))
          ((eq? next-token 'quasiquote-token)
           (list 'quasiquote (read_)))
          ((eq? next-token 'unquote-token)
           (list 'unquote (read_)))
          ((escaped? next-token)
           (frst next-token))
          (else next-token))))

(defn read-list (acc)
      (let ((next-token (read-token)))
        (cond
          ((eq? next-token 'right-paren)
           (reverse acc))
          ((eq? next-token 'left-paren)
           (read-list (cons (read-list '())
                            acc)))
          ((eq? next-token 'quote-token)
           (read-list (cons (list 'quote (read_))
                            acc)))
          ((eq? next-token 'quasiquote-token)
           (read-list (cons (list 'quasiquote (read_))
                            acc)))
          ((eq? next-token 'unquote-token)
           (read-list (cons (list 'unquote (read_))
                            acc)))
          ((escaped? next-token)
           (read-list (cons (frst next-token)
                            acc)))
          (else
            (read-list (cons next-token acc))))))
