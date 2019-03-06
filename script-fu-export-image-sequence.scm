(define (script-fu-export-image-sequence
         image prefix num-format extension dirname include-bg)

  (define (do-for-each-indexed index fn alist)
    (if (null? alist)
        nil
        (if (null? (car alist))
            #t
            (let* ((first (car alist))
                   (rest (cdr alist)))
              (fn index first)
              (do-for-each-indexed (+ index 1) fn rest)))))

  (define (for-each-indexed fn alist)
    (do-for-each-indexed 0 fn alist))

  (define (vector-last avector)
    (vector-ref avector (- (vector-length avector) 1)))

  (define (prepend-zeros num-string)
    (let ((len (string-length num-string)))
      (if (<= len 4)
          (let ((zeros (substring (number->string (expt 10 (- 4 len))) 1)))
            (string-append zeros num-string))
          num-string)))



  (let* ((new-image (car (gimp-image-duplicate image)))
         (frames (reverse
                  (vector->list
                   (cadr (gimp-image-get-layers new-image))))))

    ;; Hide all frames
    (for-each (lambda (frame) (gimp-item-set-visible frame FALSE)) frames)

    (for-each-indexed
     (lambda (index frame)

       ;; Show current frame
       (gimp-item-set-visible frame TRUE)

       ;; Export current frame
       (let* ((index-string (if (= num-format 1)
                                (prepend-zeros (number->string index))
                                (number->string index)))
              (raw-filename (string-append dirname "/" prefix index-string))
              (filename (string-append raw-filename extension))
              (background (vector-last (cadr (gimp-item-get-children frame))))
              (mode (if (= index 0) RUN-INTERACTIVE RUN-WITH-LAST-VALS)))

         (gimp-item-set-visible background include-bg)

         (gimp-file-save mode new-image frame filename raw-filename))

       ;; Hide back current frame
       (gimp-item-set-visible frame FALSE))

     frames)))


;; Registering script

(script-fu-register
 "script-fu-export-image-sequence"
 "Export Image Sequence"
 "Export all frames in image into image sequence in PNG."
 "Burhanuddin Baharuddin"
 "Copyright 2019, Burhanuddin Baharuddin"
 "March 6, 2019"
 "RGB*"
 SF-IMAGE "Image" 0
 SF-STRING "File name prefix" "image_"
 SF-OPTION "Numbering format" '("0, 1, 2...3" "0000, 0001, 0002...0003")
 SF-STRING "File extension" ".png"
 SF-DIRNAME "Location" ""
 SF-TOGGLE "Include background" FALSE)

(script-fu-menu-register "script-fu-export-image-sequence"
                         "<Image>/Filters/Animation")
