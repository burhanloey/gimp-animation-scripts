(define (script-fu-onion-skinning image layer mode)
  (let* (
         ;; Id to identify to which image the onion skins belong (used for
         ;; merging script)
         (id (car (gimp-procedural-db-temp-name)))

         ;; Allow user to select particular layer group by selecting its
         ;; children or itself
         (selected-layer-group (if (= TRUE (car (gimp-item-is-group layer)))
                                   layer
                                   (car (gimp-item-get-parent layer))))

         ;; Selected layer group position
         (selected-position (car
                             (gimp-image-get-item-position
                              image
                              selected-layer-group)))

         ;; Duplicated image for onion skinning
         (new-image (car (gimp-image-duplicate image)))

         ;; Layer groups for duplicated image, a.k.a `frames`
         (layer-groups (gimp-image-get-layers new-image))
         (num-frames (car layer-groups))
         (frames (cadr layer-groups))

         ;; Selected frame(same position as selected layer group in original
         ;; image)
         (selected-frame (vector-ref frames selected-position))

         ;; Back frame is below selected frame in GUI, therefore +1 position
         (back-frame (if (< (+ selected-position 1) num-frames)
                         (vector-ref frames (+ selected-position 1))))

         ;; Forward frame is above selected frame in GUI, therefore -1 position
         (forward-frame (if (>= (- selected-position 1) 0)
                            (vector-ref frames (- selected-position 1)))))

    (cond
     ;; Back Frame Mode
     ((= mode 0)
      (for-each
       (lambda (frame)
         ;; Remove frames other than selected and back frames.
         (if (not (or (equal? frame selected-frame)
                      (equal? frame back-frame)))
             (gimp-image-remove-layer new-image frame)))
       (vector->list frames)))

     ;; Forward Frame Mode
     ((= mode 1)
      (for-each
       (lambda (frame)
         ;; Remove frames other than selected and forward frames.
         (if (not (or (equal? frame selected-frame)
                      (equal? frame forward-frame)))
             (gimp-image-remove-layer new-image frame)))
       (vector->list frames)))

     ;; Back & forward Frames Mode
     ((= mode 2)
      (for-each
       (lambda (frame)
         ;; Remove frames other than selected, back and forward frames.
         (if (not (or (equal? frame selected-frame)
                      (equal? frame back-frame)
                      (equal? frame forward-frame)))
             (gimp-image-remove-layer new-image frame)))
       (vector->list frames))))

    ;; Raise selected frame(working layer group) to top
    (gimp-image-raise-item-to-top new-image selected-frame)

    ;; Set up opacity and background visiblity.
    (let* (
           ;; Create new bindings because of previous removal.
           (layer-groups (gimp-image-get-layers new-image))
           (num-frames (car layer-groups))
           (frames (cadr layer-groups))

           ;; Lowest available frame
           (last-frame (vector-ref frames (- num-frames 1))))

      ;; For each frame,
      (for-each
       (lambda (frame)

         ;; Show frame
         (gimp-item-set-visible frame TRUE)

         (let* (
                ;; Get all layers in the frame
                (children (gimp-item-get-children frame))
                (num-layers (car children))
                (layers (cadr children))

                ;; Lowest layer in the frame
                (background (vector-ref layers (- num-layers 1))))

           ;; Select layer that the user should work on (first layer)
           (if (equal? frame selected-frame)
               (gimp-image-set-active-layer new-image (vector-ref layers 0)))

           ;; For each layer,
           (for-each
            (lambda (layer)

              ;; Show layer
              (gimp-item-set-visible layer TRUE)

              ;; Hide the background layer except for last frame. Make layers
              ;; other than selected frame as onion skin(low opacity).
              (if (equal? layer background)
                  (if (not (equal? frame last-frame))
                      (gimp-item-set-visible layer FALSE))
                  (if (not (equal? frame selected-frame))
                      (gimp-layer-set-opacity layer 20))))

            (vector->list layers))))

       (vector->list frames)))

    ;; Set id for image and onion skin image
    (gimp-image-attach-parasite image (list "onion-skinning-id" 1 id))
    (gimp-image-attach-parasite new-image (list "onion-skinning-parent-id" 1 id))

    ;; Open onion skinning image in new display
    (gimp-display-new new-image)

    ;; Return onion skinning image for other scripts
    (list new-image)))


;; Registering script

(script-fu-register
 "script-fu-onion-skinning"
 "Onion Skinning"
 "Display before and after layers at once."
 "Burhanuddin Baharuddin"
 "Copyright 2019, Burhanuddin Baharuddin"
 "March 2, 2019"
 "RGB*"
 SF-IMAGE "Image" 0
 SF-DRAWABLE "Drawable" 0
 SF-OPTION "Mode" '("Back Frame" "Forward Frame" "Back & Forward Frames"))

(script-fu-menu-register "script-fu-onion-skinning" "<Image>/Filters/Animation")
