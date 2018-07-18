;;; mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10
;;; ============================================================
;;; Author       : David Peebles
;;; Address      : Department of Psychology
;;;              : University of Huddersfield,
;;;              : Queensgate, Huddersfield,
;;;              : HD1 3DH, UK.
;;;              : d.peebles@hud.ac.uk
;;; ============================================================
;;; Filename     : sart.lisp
;;; Version      : 2.0
;;; Description  : ACT-R 7.0 model of SART experiment
;;;              : using the virtual interface
;;; ============================================================

(defvar t1 nil)
(defvar t2 nil)
(defvar init-list '("1" "2" "3" "4" "5" "6" "7" "8" "9"
                    "1" "2" "3" "4" "5" "6" "7" "8" "9"
                    "1" "2" "3" "4" "5" "6" "7" "8" "9"
                    "1" "2" "3" "4" "5" "6" "7" "8" "9"
                    "1" "2" "3" "4" "5" "6" "7" "8" "9"))

(defvar *training-list* '("1" "2" "3" "4" "5" "6" "7" "8" "9"))

(defvar *data* nil)
(defvar *click-time* nil)

;; ==================
;;
;; ==================

(defun schedule-presentation (items)
  (when *data*
    (push *click-time* *data*))
  (unless (null items)
    (add-text-to-exp-window :text (car items) :x 145 :y 145)
;;    (format t "Stim: ~A~%" (car items))
    (proc-display)
    (setf t1 (get-time))
    (setf *click-time* nil)
    (push (car items) *data*)
    (schedule-event-relative .25 #'schedule-clear :params (list (cdr items)))))

;; ==================
;;
;; ==================

(defun schedule-clear (items)
  (clear-exp-window)
  (proc-display)
  (when items
    (schedule-event-relative .9 #'schedule-presentation :params (list items))))

;; ==================
;;
;; ==================

(defmethod rpm-window-click-event-handler ((window rpm-window) pos)
  (setf t2 (get-time))
  (setf *click-time* (- t2 t1)))

;; ==================
;;
;; ==================

(defun analyze-data (data &optional display)
  (let ((d nil)
        (z0 0) (z1 0)
        (c0 0) (c1 0) (c2 0) (c3 0) (c4 0)
        (t0 0) (t1 0) (t2 0) (t3 0) (t4 0)
        (e1 0) (e2 0)
        (last-trial nil)
        (last-time nil))
    ;;
    (while data
      (push (list (pop data) (pop data)) d))

    ;;
    (dolist (x (subseq d 18))
      (let ((this-trial (second x))
            (this-time (first x)))

        (when display
          (format t "~S   ~S   ~S~%"
            this-trial
            (if (numberp this-time) this-time 0)
            (if (string-equal "3" this-trial) 1000 0)))

        (if (equal this-trial "3")
            (incf z0)
            (incf z1))

        (when (and (numberp this-time) (not (equal this-trial "3")))
          (incf c0)
          (incf t0 this-time))

        (when (and (equal this-trial "3")
                   (numberp this-time))
          (incf e1))
        (when (and this-trial
                   (not (equal this-trial "3"))
                   (null this-time))
          (incf e2))

        ;; ===================================
        (when (equal "3" this-trial)
          (if (and last-trial
                   (not (equal last-trial "3")))
              (if (and (numberp last-time)
                       (numberp this-time))
                  (progn (incf c2)
                    (incf t2 last-time))
                (if (numberp last-time)
                    (progn
                      (incf c1)
                      (incf t1 last-time))))))

        ;; ====================================
        (when (and (numberp this-time)
                   (equal "3" last-trial)
                   (not (equal "3" this-trial)))
          (if (numberp last-time)
              (progn
                (incf c4)
                (incf t4 this-time))
            (progn
              (incf c3)
              (incf t3 this-time))))

        (setf last-trial this-trial)
        (setf last-time this-time)))

    (list (list (if (zerop c0) 0 (/ t0 c0))
                (if (zerop c1) 0 (/ t1 c1))
                (if (zerop c2) 0 (/ t2 c2))
                (if (zerop c3) 0 (/ t3 c3))
                (if (zerop c4) 0 (/ t4 c4)))
          (list c0 c1 c2 c3 c4 e1 e2))))

;; ==================
;;
;; ==================

(defconstant +rt-data+ '(384 333 351 364))
(defconstant +error-data+ '(17.6 6 17.3 6.07))

;; ==================
;;
;; ==================

(defun stats (results)
  (mean-deviation +rt-data+ (cdar results))
  (correlation +rt-data+ (cdar results))
  (format t "RT: ~A~%" (cdar results))
  (mean-deviation +error-data+ (subseq (second results) 1 5))
  (correlation +error-data+ (subseq (second results) 1 5))
  (format t "ERR: ~A~%" (subseq (second results) 1 5))
  results)

;; ==================
;; run the experiment
;; ==================

(defun runexp (display)
  (let* ((stimuli (append (permute-list *training-list*)
                          (permute-list *training-list*)
                          (permute-list init-list)
                          (permute-list init-list)
                          (permute-list init-list)
                          (permute-list init-list)
                          (permute-list init-list)))
         (window (open-exp-window "SART" :visible nil)))
    (reset)
    (setf *data* nil)
    (install-device window)
    (schedule-presentation stimuli)
    (run 10000 :real-time nil)
    (push *click-time* *data*)
    (analyze-data *data* display)))

;; ==============================================
;; Code to run the experiment repeatedly and
;; collect data and run an analysis
;; ==============================================

(defun collect-data (n &optional (display nil))
  (let ((rts (list 0 0 0 0 0))
        (counts (list 0 0 0 0 0))
        (errors (list 0 0 0 0 0 0 0))
        (spread nil))
    (dotimes (i n)
      (let* ((result (runexp display))
             (times (car result))
             (nums (second result)))
        (push (list (* 1.0 (first times)) (sixth nums) (seventh nums)) spread)
        (setf rts (mapcar #'+ times rts))
        (setf counts (mapcar #'(lambda (x y) (if (zerop x) y (1+ y))) nums counts))
        (setf errors (mapcar #'+ nums errors))))
    (pprint spread)
    (stats (list (mapcar #'(lambda (x y) (if (zerop y) 0.0 (* 1.0 (/ x y)))) rts counts)
                 (mapcar #'(lambda (x) (* 1.0 (/ x n))) errors)))))

;; ==================================
;; The actual ACT-R model begins here
;; ==================================

(clear-all)
(define-model sart

;; ================================
;; set ACT-R global parameters
;; ================================

(sgp :v nil                  ;; verbose flag controls output
     :trace-detail low     ;;
     :esc t                ;; enable subsymbolic computation
     :ul  t                ;; enable utility learning
     :randomize-time t     ;;
     :alpha .2            ;; utility learning rate
     :egs .4              ;; S value of the noise in utility
     :imaginal-delay 0.02 ;; Time (in s) to create an imaginal chunk (default = 0.02)
)

(sgp :buffer-trace nil
     :buffer-trace-step .025
     :traced-buffers (production goal visual-location visual manual imaginal))

;; This is a test to see if the RTs can be modelled by setting the
;; general :motor-initiation-time to 10ms and higher for when the
;; deliberate check production

;; ==================
;; define chunk types
;; ==================

(chunk-type do-sart-trial state target-number response check-it)
(chunk-type current-number value response)

;; ===============================
;; add facts to declarative memory
;; ===============================

(add-dm
 (start isa chunk) (attend-number isa chunk) (clicked isa chunk)
 (check-response isa chunk) (check-for-three isa chunk) (yes isa chunk)
 (not-clicked isa chunk) (g1 isa do-sart-trial target-number "3" state start))

(goal-focus g1)
(start-hand-at-mouse)

;; ================
;; production rules
;; ================

;; ================================================
;; IF   you're starting a SART trial
;; AND  a new object's location has been detected
;; AND  the vision module is ready
;; THEN attend to the object at that location
;; ==========================================

(p attend-number
  =goal>
   state start
  =visual-location>
  ?visual>
   state free
 ==>
  +visual>
   cmd move-attention
   screen-pos =visual-location
  =goal>
   state attend-number
   !eval! (sgp :motor-initiation-time 0.00)
   !eval! (sgp :motor-feature-prep-time 0.00)
   !eval! (sgp :motor-burst-time 0.00))

;; ===========================================================
;; IF   you're attending to a number
;; AND  you are not explicitly verifying first
;; THEN click the mouse
;; AND  check that your response is appropriate for the number
;; ===========================================================

(p enc-click
  =goal>
   state attend-number
   check-it nil
  =visual>
   isa text
   value =number
  ?manual>
   state free
  ?imaginal>
   state free
 ==>
  +manual>
   cmd click-mouse
  +imaginal>
   isa current-number
   value =number
   response clicked
  =goal>
   state check-response)

;; ===========================================================
;; IF   you're attending a number
;; AND  failed to encode it before the mask appeared
;; THEN don't click the mouse
;; AND prepare for the next trial
;;
;; Can't classify as success or failure
;; ===========================================================

(p missed-no-click
  =goal>
   state attend-number
   target-number =target
  ?visual>
   state error
 ==>
  +goal>
   state start
   target-number =target)

;; ===========================================================
;; IF   you're attending a number
;; AND  failed to encode it before the mask appeared
;; THEN click the mouse
;; AND prepare for the next trial
;;
;; Can't classify as success or failure
;; ===========================================================

(p missed-click
  =goal>
   state attend-number
   target-number =target
  ?visual>
   state error
  ?manual>
   state free
 ==>
  +manual>
   cmd click-mouse
  +goal>
   state start
   target-number =target)

;; =============================================
;; IF   you're attending to a number
;; THEN check the number before responding to it
;; =============================================

(p enc-check
  =goal>
   state attend-number
  =visual>
   isa text
   value =number
  ?imaginal>
   state free
 ==>
  +imaginal>
   isa current-number
   value =number
  =goal>
  state check-for-three
  !eval! (sgp :motor-initiation-time 0.05)
  !eval! (sgp :motor-feature-prep-time 0.05)
  !eval! (sgp :motor-burst-time 0.05))

;; ===========================================================
;; IF   you're checking the number before responding to it
;; AND  the number is 3
;; THEN don't click the mouse
;; AND  check that your response is appropriate for the number
;; ===========================================================

(p stim-three
  =goal>
   state check-for-three
   target-number =target
  =imaginal>
   isa current-number
   value =target
 ==>
  =goal>
   state check-response
  =imaginal>
   response not-clicked)

;; ===========================================================
;; IF   you're checking the number before responding to it
;; AND  the number is not 3
;; THEN click the mouse
;; AND  check that your response is appropriate for the number
;; ===========================================================

(p stim-not-three
  =goal>
   state check-for-three
   target-number =target
  =imaginal>
   isa current-number
 - value =target
  ?manual>
   state free
 ==>
  +manual>
   cmd click-mouse
  =goal>
   state check-response
  =imaginal>
   response clicked)

;; =============================================
;; IF   you're checking your response
;; AND  you've incorrectly responded to the target
;; THEN set a new goal to do another trial
;;
;; This is a failure
;; =============================================

(p incorrect-target
  =goal>
   state check-response
   target-number =target
  =imaginal>
   isa current-number
   value =target
   response clicked
 ==>
  +goal>
   state start
   target-number =target)

;; =============================================
;; IF   you're checking your response
;; AND  you've incorrectly responded to the target
;; THEN set a new goal to do another trial
;; AND  explicitly check the next trial
;;
;; This is a failure
;; =============================================

(p incorrect-double-check
  =goal>
   state check-response
   target-number =target
  =imaginal>
   isa current-number
   value =target
   response clicked
 ==>
  +goal>
   state start
   target-number =target
   check-it yes)

;; =======================================================
;; IF   you're checking your response
;; AND  you've correctly witheld a response to the target
;; THEN set a new goal to do another trial
;;
;; This is a success
;; =======================================================

(p correct-target
  =goal>
   state check-response
   target-number =target
  =imaginal>
   isa current-number
   value =target
   response not-clicked
 ==>
  +goal>
   state start
   target-number =target)

;; ===================================================
;; IF   you're checking your response
;; AND  you've correctly responded to a non-target
;; THEN set a new goal to do another trial
;;
;; This is a success
;; ===================================================

(p correct-non-target
  =goal>
   state check-response
   target-number =target
  =imaginal>
   isa current-number
 - value =target
   response clicked
 ==>
  +goal>
   state start
   target-number =target)

;; =========================
;; set production parameters
;; =========================

;; Since the subjects know that targets are rare and there's no
;; feedback, assume that there's a preference for clicking over not
;; clicking when failing to encode the number

;(spp missed-it-dont-click :u 10)
(spp missed-click :u 3)
;(spp encode-and-click :u 10)
;(spp encode-and-check :u 10)

;; ================================
;; Rewards for response productions
;; ================================

;; Correct responses propagate positive reward
(spp correct-non-target :reward 3)
(spp correct-target :reward 3)

;; Incorrect responses propagate negative or no reward
(spp incorrect-double-check :reward 0)
(spp incorrect-target :reward 0)

)
