;; decreasing a number by 1
(define (decrement x)
  (- x 1)
  )
;; increasing a number by 1
(define (increment x)
  (+ x 1)
  )
   
;; Function retrieving the x coordinate
(define (x-pos state)(car (cadr state)))
;; Function retrieving the y coordinate
(define (y-pos state) (cadr (cadr state)))
   
;;; north? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (north? state)
  (if (eqv? 'north (orientation? state)) #t #f)
  )
   
;;; wall? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (wall? state) (cond
          ((eqv? (orientation? state) 'north)(eqv? 'w (position_at (car state) (x-pos state) (decrement (y-pos state)))))
          ((eqv? (orientation? state) 'south)(eqv? 'w (position_at (car state) (x-pos state) (increment (y-pos state)))))
          ((eqv? (orientation? state) 'east)(eqv? 'w (position_at (car state) (increment (x-pos state)) (y-pos state))))
          ((eqv? (orientation? state) 'west)(eqv? 'w (position_at (car state) (decrement (x-pos state)) (y-pos state))))
          )
        )
;;; returns item in the maze at x-th and y-th position
(define (position_at state x y)
(define (ith_value list index)
  (if (eqv? 0 index) (car list) (ith_value (cdr list) (decrement index)))
  )
(ith_value (ith_value state y) x)
  )
;;; taking a step forward
(define (step state)
  (if (wall? state) (state)
   (cond ((eqv? (orientation? state) 'south) (list (car state) (list (x-pos state) (increment (y-pos state))) (orientation? state)))
         ((eqv? (orientation? state) 'east) (list (car state) (list (increment (x-pos state)) (y-pos state)) (orientation? state)))
         ((eqv? (orientation? state) 'north)(list (car state) (list (x-pos state) (decrement (y-pos state))) (orientation? state)))
         ((eqv? (orientation? state) 'west) (list (car state) (list (decrement (x-pos state)) (y-pos state)) (orientation? state)))
         )
   )
  )
   
;;; turn left ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (turn-left state)
  (cond
    ((eqv? (orientation? state) 'north)(list (car state) (cadr state) 'west))
   ((eqv? (orientation? state) 'west)(list (car state) (cadr state) 'south))
    ((eqv? (orientation? state) 'south)(list (car state) (cadr state) 'east))
    ((eqv? (orientation? state) 'east)(list (car state) (cadr state) 'north))
    ;(else display "something went wrong during turning left")
   
    )
  )
   
(define (orientation? state) ;;; tells me which side am I facing
  (caddr state))
   
;; IF CALL ;;;;;;;;;;;;;;;;
(define (if_call actions state expr program limit counter threshold)
         (cond
           ((eqv? (car (cadr expr)) 'wall?)(if (wall? state)
                                               (resolve actions state (cons (cadr (cadr  expr)) (cddr expr)) program limit counter threshold)
                                               (resolve actions state (cons (caddr (cadr  expr)) (cddr expr))  program limit counter threshold)))
           ((eqv? (car (cadr expr)) 'mark?)(if (mark? state)
                                               (resolve actions state (cons (cadr (cadr  expr)) (cddr expr)) program limit counter threshold)
                                               (resolve actions state (cons (caddr (cadr  expr)) (cddr expr))  program limit counter threshold)))
           ((eqv? (car (cadr expr)) 'north?)(if (north? state)
                                                (resolve actions state (cons (cadr (cadr  expr)) (cddr expr)) program limit counter threshold)
                                                (resolve actions state (cons (caddr (cadr  expr)) (cddr expr)) program limit counter threshold)))
            (#t (display "err"))
         )
   )
   
(define (ordinary_direction? argument)
  (or (eqv?  argument 'stop)(eqv? argument 'get-mark)(eqv? argument 'put-mark)(eqv? argument 'step)(eqv? argument 'turn-left))
  )
   
(define (mark? state)
   (< 0 (position_at (car state) (x-pos state) (y-pos state)))
  )
;;; CHANGING A 2D LIST
   
(define (change_array maze x y replacement)
  (define (change_list maze_part index replacement)
  (if (null? maze_part)
    '()
    (cons
      (if (zero? index)
        replacement
        (car maze_part))
      (change_list (cdr maze_part) (decrement index) replacement))))
  (if (null? maze)
      '()
      (cons
       (if (zero? y)
           (change_list (car maze) x replacement)
           (car maze))
           (change_array (cdr maze) x (decrement y) replacement))))
       
   
;;; PUT AND GET MARK
(define (get-mark state)
  (list (change_array (car state)  (x-pos state) (y-pos state) (decrement (position_at (car state) (x-pos state) (y-pos state)))) (cadr state) (orientation? state))
)
(define (put-mark state)
  (list (change_array (car state)  (x-pos state) (y-pos state) (increment (position_at (car state) (x-pos state) (y-pos state)))) (cadr state) (orientation? state))
)
   
;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;; ;;; ; ;; 
   
(define (join start end)
  (if (null? start) end (if (pair? start)(cons (car start) (join (cdr start) end))(cons start end))
  ))
   
   
   
(define (extract_procedure  program expr)
  (if (pair? expr)(if(eqv? (car expr) (cadar program))(cons (caddar program) (list 'stop))(extract_procedure  (cdr program) expr))(extract_procedure  program (list expr)))
 ;; (cond ((not(pair? expr)) (extract_procedure  program (list expr)))
     ;;   ((eqv? (cadar program) (car expr)) (cons (caddar program) (list 'stop)))
    ;;    (#t (extract_procedure  (cdr program) expr))))
  )
   
; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; 
;;; Higher order functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (resolve actions state expr program limit counter threshold)
  (if (needs_adjusting? actions state expr program limit counter threshold)(ratify actions state expr program limit counter threshold)
  (cond
  ;; 3. IF ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ((eqv? 'if (car expr))
         (if_call actions state expr program limit counter threshold)
         )
        ;; 4. checking ordinary commands and recall the function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (else(if(ordinary_direction? (car expr))
         (cond
           ((eqv? (car expr) 'put-mark)(resolve (join actions (list (car expr)))(put-mark state)(cdr expr)program limit counter (decrement threshold)))
           ((eqv? (car expr) 'get-mark)(if (mark? state)(resolve (join actions (list (car expr)))(get-mark state)(cdr expr) program limit counter (decrement threshold))(list actions state)))
           ((eqv? (car expr) 'turn-left)(resolve (join actions (list(car expr)))(turn-left state)(cdr expr)program limit counter (decrement threshold)))
           ((eqv? (car expr) 'step)(if (wall? state)(list actions state)(resolve (join actions (list (car expr)))(step state)(cdr expr) program limit counter (decrement threshold))))
           ((eqv? (car expr) 'stop)(resolve actions state  (cdr expr)  program  limit (decrement counter) threshold)) ;;;;; not sure if shouldnt decrement
           ;;(#t (display (cond (car expr))))
         )(resolve actions state (append (append (extract_procedure program (car expr)) (cdr expr)) 'stop) program limit (increment counter) threshold)
        ))
           )))  
;;; simulate call
   
;;; ADJUSTING INPUT
(define(needs_adjusting? actions state expr program limit counter threshold)
  (or (or (> counter limit)(> 0 threshold) (null? expr))(not(list? expr))(null? (car expr))(pair? (car expr)))  
)
(define (ratify actions state expr program limit counter threshold)
  (cond
    ((or (> counter limit)(null? expr) (> 0 threshold))(list actions state)) ;; if we ran out of steps
    ((not(list? expr))(resolve actions state (list expr) program limit counter threshold)) ;; listify
    ((null? (car expr))(resolve actions state (cdr expr) program limit counter threshold)) ;; empty car
    (#t ;; pair checking
     (cond
       ((not (null? (cdar expr)))(resolve actions state (join (join (list (caar expr))(list(cdar expr))) (cdr expr)) program limit counter threshold))
       (#t (resolve actions state (join (list (caar expr))(cdr expr)) program limit counter threshold))
   
      )
       ) 
    )
)
(define (simulate state expr program limit threshold)
     
  (resolve '() state expr program limit 0 threshold )
  ;;(display (resolve '() state expr program limit 0 threshold ))
  )
   
   
   
   
   
(define (evaluate programs pairs threshold stack)
  (second_resolve '() programs pairs threshold stack)
  )
   
   
(define (second_resolve actions programs pairs threshold stack)
  (if(null? programs) actions
      (let ((current (examined '(0 0 0 0) (car programs) pairs threshold stack)) )
           (if(not(null? current))(second_resolve (sortify actions current) (cdr programs) pairs threshold stack)
              (second_resolve actions (cdr programs) pairs threshold stack)
   
             )
           )
     )
  )
   
;;;;;;;;;;;;
   
;;; Manhattan
;; flatten
(define (my_flatten maze)
  (cond
    ((list? maze) (apply append (map my_flatten maze)))
    (#t (list maze))
    )
  )
   
   
;;;;;;;;;;;;;;;;;; SORTINGS ;;;;;;;;;;;;;;;;;;
(define (sortify where what)
  (define (manhattan_swap? where what record)
    (if (or (> (caaar where) (caar what) ) (< (caaar where) (caar what))) #t #f)
  )
   
  (define (conf_dist_swap? where what record)
    (if (or (> (cadaar where) (cadar what) ) (< (cadaar where) (cadar what))) #t #f)
  )
   
  (define (manhattan_swap where what record)
   
    (if (< (caaar where) (caar what)) (sort (cdr where) what (join record (list (car where)))) (join record (cons what where)))
   
  )
  (define (conf_dist_swap where what record)
    
    (if (< (cadaar where) (cadar what)) (sort (cdr where) what (join record (list (car where)))) (join record (cons what where)))
  )
  (define (proglen_swap? where what record)
    (if (or (> (car (cddaar where)) (caddar what) ) (< (car (cddaar where)) (caddar what))) #t #f)
  )
   
  (define (proglen_swap where what record)
   
    (if (< (car (cddaar where)) (caddar what)) (sort (cdr where) what (join record (list (car where)))) (join record (cons what where)))
  )
  (define (steps_swap? where what record)
    (if (or (> (cadr (cddaar where)) (cadr (cddar what) )) (< (cadr (cddaar where)) (cadr (cddar what)))) #t #f)
  )
  (define (steps_swap where what record)
   
    (if (< (cadr (cddaar where)) (cadr(cddar what))) (sort (cdr where) what (join record (list (car where)))) (join record (cons what where)))
  )
  (define (the_same? where what record)
    (if (eqv? where what) #t #f)
    ;(display what)(display where)
  )
  (define (the_same where what record)
    (join record(cons what where))
  )
  (define (sort where what record)
  (cond
    ;;((null? what) where) ;; doubtful
    ((null? where) (join record (list what))) 
    ((null? (car where)) (list what))
    ((manhattan_swap? where what record) (manhattan_swap where what record))
    ((conf_dist_swap? where what record) (conf_dist_swap where what record))
    ((proglen_swap? where what record) (proglen_swap where what record))
    ((steps_swap? where what record) (steps_swap where what record))
    ;;((the_same? where what record)(the_same where what record))
    (#t (join record (cons what where)))))
  (sort where what '())
  )
   
(define (join start end)
  (if (null? start) end (if (pair? start)(cons (car start) (join (cdr start) end))(cons start end))
  ))
   
(define (passes? current)
  (if (null? current) #f #t)
  )
;;;;;;    Manhattan  ;;;;;;;;;;;;;;;
   
(define (manhattan first second)
  (cond
    ((null? first) 0)
    ( (pair? (car first))(+ (manhattan (car first) (car second))(manhattan (cdr first)(cdr second))))
    ((integer? (car first))(+ (manhattan (cdr first)(cdr second)) (abs(- (car first) (car second)))))
    (#t (manhattan (cdr first) (cdr second)))
    )
   
  )
(define (wrong_manhattan? first second threshold)
  (if (> (manhattan (car first) (car second)) (car threshold)) #t #f)
  )
   
;;;;;;;   Configuration distance ;;;;;;;;;;
(define (distancing first second)
    (define (bonus first second)
      (if (eqv? (caddr first) (caddr second)) 0 1)
      )
  (+ (bonus first second)(abs (- (caadr first) (caadr second)))(abs (- (cadadr first) (cadadr second)))))
   
(define (too_distant? first_state second_state threshold_part)
  (if (>(distancing first_state second_state)threshold_part) #t #f)
  )
;;;;    Program length   ;;;;;;;;
(define (too_long? action_part programs threshold)
  (if (>(if (eqv? (caddr action_part) 0)(length_check2 (my_flatten programs)) 0) (caddr threshold)) #t #f)
  )
;;;;;    Step count    ;;;;;;;;;;
   
(define (too_steps? trial_part threshold_part)
  (if (> (length trial_part) threshold_part) #t #f)
  )
   
(define (listify trial pairs actions program)
   
  (joining_lists2 '() (list (manhattan (caadr trial) (caadar pairs)) (distancing (cadr trial) (cadar pairs)) (if (zero? (car(cddr actions)))(length_check2 (my_flatten program)) 0) (length (car trial))) actions)
  )
   
(define (joining_lists2 listy a b)
  (cond
    ((null? a) listy)
    (#t (cons (+ (car a) (car b)) (joining_lists2 listy (cdr a) (cdr b))))
   
    )
  )
(define (examined actions programs pairs threshold stack)
  ;(display actions)
  (if (null? pairs) (cons actions (list programs))
      (let ((trial (simulate (caar pairs) `(start) programs stack (cadddr threshold))))
        ;(display pairs)
        (cond
          ;; if manhattan doesnt holdup return;
          ;((wrong_manhattan? (cadr trial) (cadar pairs) threshold) '())
          ;; if configuration distance too long, return
          ;((too_distant? (cadr trial) (cadar pairs) (cadr threshold)) '())
          ; if it is too long, return
         ;((too_long? actions programs threshold) '())
          ;; too many steps?, return
          ;((too_steps? (car trial) (cadddr threshold)) '())
          ;; else,continue
          (else (let((result (listify trial pairs actions programs)))
                  (if (or (>(cadr result) (cadr threshold)) (>(car result) (car threshold)) (> (caddr result) (caddr threshold)) (> (cadddr result) (cadddr threshold))) '() (examined result programs (cdr pairs) threshold stack))
   
                  ))
          )
       ))
    )
   
;;;;;;;;;;;;;;;;;;;;;; bbeware;;;;;;;;;;;;;
(define (length_check2 program)
  (if (null? program) 0
      (if
        (or (eqv? (car program) 'if) (eqv? (car program) 'procedure))(length_check2 (cdr program))
         (increment (length_check2 (cdr program)))
        )
  )
 )
 
(define pop-zero '(
((procedure start (step)))
((procedure start (get-mark)))
((procedure start ((if wall? put-mark (step step)))))
((procedure start (turn-left turn-left step)))
((procedure start ((if north? put-mark (step start)))))
((procedure start (put-mark (if wall? (turn-left turn-left) ()) step get-mark)))
((procedure start (put-mark (if mark? (turn-left turn-left turn-left turn-left step turn-left) ()) step put-mark)))
((procedure start ((if north? (put-mark) (turn-left turn-left)))))
((procedure start ((if wall? put-mark (step start)))))
((procedure start ((if wall? put-mark (step start turn-left step turn-left turn-left turn-left step)))))
((procedure start (turn-left turn-left turn-left step)))
((procedure start ((if wall? put-mark (start step turn-left turn-left step turn-left turn-left)))))
((procedure start ((if north? (put-mark) (turn-left turn-left turn-left)))))
((procedure start (get-mark (if wall? (turn-left turn-left) ()) step put-mark)))
((procedure start ((if north? (turn-left) (put-mark) ))))
((procedure start (step step start turn-left)))
((procedure start (step turn-left step turn-left)))
((procedure start (turn-left step)))
((procedure start ((if wall? get-mark (step start turn-left turn-left step turn-left turn-left)))))
((procedure start ((if wall? (turn-left turn-left step step) (step)))))
((procedure start ((if wall? (turn-left start) (step step put-mark)))))
((procedure start (step start)))
((procedure start (put-mark)))
((procedure start ((if wall? put-mark step))))
((procedure start (start)))
((procedure start ((if wall? put-mark (step start turn-left turn-left step turn-left turn-left)))))
((procedure start (put-mark (if mark? (turn-left turn-left) ()) step put-mark)))
((procedure start (() turn-left turn-left turn-left)))
((procedure start (turn-left turn-left turn-left)))
 ((procedure start ((if wall? () (step start step)) put-mark)))
((procedure start ((if north? (turn-left turn-left) (put-mark) ))))
((procedure start ((if wall? () (step start step) put-mark))))
((procedure start (step)))
((procedure start (step start)))
 ((procedure start ((if north? (step turn-left step) (get-mark step turn-left)))))
 ((procedure start (turn-left (if wall? (turn-left step) (turn-left)))))
((procedure start ((if north? (start) ()) turn-left start)))
))
   
   
;;;;;;;       Random Number generator       ;;;;;;;;
   
(define (congruential-rng seed)
  (let ((a 16807 #|(expt 7 5)|#)
        (m 2147483647 #|(- (expt 2 31) 1)|#))
    (let ((m-1 (- m 1)))
      (let ((seed (+ (remainder seed m-1) 1)))
        (lambda (b)
          (let ((n (remainder (* a seed) m)))
            (set! seed n)
            (quotient (* (- n 1) b) m-1)))))))
(define random (congruential-rng 25974))
   
   
   
;;;;   SELECTION ;;;;;;;
(define selection-target 40);; target number of programs to choose
(define (selection population)
  ;;(define selection-target 20);; target number of programs to choose
  (define best-count 8) ;; tells me how many programs are for certain going to get into the next population
  (define threshold-step 50) ;; by this amount our threshold decreases
  (define (selecting programs-left target counter probability new-gen);; so counter decides whether the current number is one of the best ones or not...
    (cond
      ((null? programs-left) new-gen) ;; no more selection left, we return
      ((= target counter) new-gen) ;; we have picked the right amount of programs .....CONSIDER REMOVINGS
      (#t (let ((randint (random 1000))(current (car programs-left))(rest (cdr programs-left)))
            (cond
              ((or (> probability randint) (< counter best-count))(selecting rest target (increment counter) (- probability threshold-step) (append new-gen (list (cadr current))))) ;;; one of the fittest programs or chosen ones
              (#t (selecting rest target counter (- probability threshold-step) new-gen)) ;; this program wasnt chosen and we proceed to other ones
              )
       )
    )
  ))
  (define (preselect population selection-target new-gen)
    (let ((selection (selecting population selection-target 0 1000 new-gen)))
      (if (< (length selection) selection-target) (preselect population selection-target (append new-gen selection)) selection)
    ))
 ;;(newline) (newline) (newline) (newline)
  ;;(display (selecting population selection-target 0 100 '()))
  ;; (newline) (newline) (newline) (newline) (newline) (newline)
  ;;(selecting population selection-target 0 100 '())
  (preselect population selection-target '())
  )
;;;; MUTATION ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (define (random-move) 
  (let ((die (random 80)))
    (cond
      ;;((> die 90)`())
      ((> die 60)`step)
      ((> die 40)`turn-left)
      ((> die 20)`put-mark)
      (#t `get-mark)
      )
  ))
(define (get-random-if)   ;; generates a random if
  (define (if-construct)
  (let ((randint (random 3)))
  (cond
    ((eqv? 0 randint)`(if mark? () ()))
    ((eqv? 1 randint)`(if north? () ()))
    ((eqv? 2 randint)`(if wall? () ()))
    )
  ))
  (let ((construct (if-construct)))
  (append (append (cons (append '() (car construct)) (list(cadr construct))) (list(list(random-move)))) (list(list(random-move))))
  ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; append instruction to the back
(define (append-mutation program instruction)
(let ((ending (if (list? (caddar program)) (append (caddar program) (list(append '() 'step))) (append (list(caddar example)) (list(append '() (random-move)))))))
  (list (append (list(car (car program)))(append (list(cadr (car program))) ending)))
 ))
   
;;;;;;; inserting a mutation
   
(define (insert-mutation program)
  (define (inserting where at what)
    (define (replace origin position what)
  (define (replacing origin position what new)
    (if (null? origin) new
        (if(> position 0)
          (replacing (cdr origin) (decrement position) what (append new (list (car origin))))
          (append (append new (list what))  (cdr origin))
          ) 
    )
   )
  (replacing origin position what '())
  )
    (append (list(car where)) (append (list(cadr where)) (list(replace (caddr where) at what))))
    )
  (let (( randpos (random (length (caddar program)))) (random-insert (random 3)))
    (if (eqv? random-insert 0) (list(inserting (car program) randpos (get-random-if)))
        (list(inserting (car program) randpos (random-move))))
    )
)
   
;;;;;;;; Crossing OVER ;;;;;;;;;;;;;;;
(define (cross population)
  #|I didnt end up using cross-over, as it was slowing the program down and detracted from its performance|#
  (define (do-cross result first second)
    (let ((randint (random 2)))
    (cond
      ((or (null? first) (null? second)) result)
      ((eqv? randint 0)(do-cross (append result (list(car first))) (cdr first) (cdr second)))
      (#t (do-cross (append result (list(car second))) (cdr first) (cdr second)))
      ))
    )
  (define (crossify first second) 
    (append (list(list(append (append (list(caar first)) (list(cadar first))) (list(do-cross '() (caddar first) (caddar second))))))
                    (list(list(append (append (list(caar second)) (list(cadar second))) (list(do-cross '() (caddar first) (caddar second)))))))
       
    )
  (define (crossover population new-pop)
    (cond
      ((null? population) new-pop)
      (#t (crossover (cdr(cdr population)) (append new-pop (crossify (car population) (cadr population)))))
      )
    )
  (crossover population '())
  population
  )
;;;;;; ;;;;;;;;; ;;;;;;;; MUTATION ;;;;;;;;; ;;;;;;;;; ;;;;;;;; ;;;;;; ;;;;;;;;;
   
(define (mutation population)
  (define (mutate? population mutated)
    (if (null? population) mutated
          (let ((current (car population)) (die (random 1000)) (current-length (length (caddar (car  population )))))
            (cond
              ((null? current) mutated)
              ;;
              ((> 100 die)(mutate? (cdr population)(append mutated (list(list(append (list(caar current)) (append (list (cadr(car current))) (list(append (caddr (car current)) (list 'step))))))))))
              ((> 4 current-length) (mutate? (cdr population)(append mutated (list(list(append (list(caar current)) (append (list (cadr(car current))) (list(append (caddr (car current)) (list 'step))))))))))
              (#t (mutate? (cdr population) (append mutated (list(insert-mutation current)))))
          ))
    ))
  (mutate? population '())
  )
;;;;;;  Evolution  ;;;;;;;;;
(define (evolve pairs threshold stack)
  (define (evolution pop-zero pairs threshold stack)
    (let ((generation (evaluate pop-zero pairs threshold stack)))
      ;;(newline)
      ;;(newline)
      ;;(display generation)
      ;;(newline)
      ;;(newline)
      (cond
        ((null? generation)  (evolution (mutation(selection generation)) pairs threshold stack)) ;; if no luck searching, go back a step and try again
        (#t
         (newline)(newline)
         (display (selection generation))
         (newline)(newline)
         (display generation)
        (cond
          ((and (eqv? 0 (car (car (cadr generation)))) (eqv? 0 (cadr(car (cadr generation))))) (display (cadr generation))(newline)(cadr generation))
          ((and (eqv? 0 (car (car (car generation)))) (eqv? 0 (cadr(car (car generation))))) (display (car generation))(newline)(car generation))
          (#t (evolution #|(cross|#(mutation(selection(append generation (car generation))))#|)|#pairs threshold stack))
        )
        ))))
  (evolution pop-zero pairs threshold stack)
)
