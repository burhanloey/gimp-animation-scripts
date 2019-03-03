(define (script-fu-resized-playback-with-onion-skinning image layer size mode)
  (define (make-onion-skin image layer mode)
    "Same procedure as `script-fu-onion-skinning` except without opening new
display"
    (let* (
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

      ;; Return onion skinning image for other scripts
      (list new-image)))


  (define (resize-and-play image size)
    "Same procedure as `script-fu-resized-playback` except without duplicate"
    (let* (
           ;; Determines scale from size(index). scale = 2 ^ index
           (scale (expt 2 size))

           ;; Get width and height
           (width (car (gimp-image-width image)))
           (height (car (gimp-image-height image)))

           ;; Scale width and height
           (scaled-width (* width scale))
           (scaled-height (* height scale))

           ;; Create empty layer for animation playback
           (layer (car (gimp-layer-new image
                                       scaled-width scaled-height
                                       RGB-IMAGE "Layer 1" 100 NORMAL-MODE))))

      ;; Set interpolation to none to avoid blur
      (gimp-context-set-interpolation INTERPOLATION-NONE)

      ;; Scale image
      (gimp-image-scale image scaled-width scaled-height)

      ;; Update progress bar
      (gimp-progress-update 1.0)

      ;; Run animation playback
      (plug-in-animationplay RUN-INTERACTIVE image layer)

      ;; Return scaled image for other scripts
      (list image)))


  (let* ((width (car (gimp-image-width image)))
         (height (car (gimp-image-height image)))

         ;; New image to put all onion skins together
         (new-image (car (gimp-image-new width height RGB)))

         ;; Layers that will be onion skinned
         (layers (gimp-image-get-layers image)))

    ;; For each layer,
    (for-each
     (lambda (layer)
       (let* ((name (car (gimp-item-get-name layer)))

              ;; Do `script-fu-onion-skinning` procedure to make onion-skin
              (onion-skin (car (make-onion-skin image layer mode)))

              ;; Make the onion skin image as a layer
              (onion-skin-layer (car
                                 (gimp-layer-new-from-visible
                                  onion-skin
                                  new-image
                                  name))))

         ;; Insert layer at the end (append)
         (gimp-image-insert-layer new-image onion-skin-layer 0 (car layers))))

     (vector->list (cadr layers)))

    ;; Resize and run animation playback
    (resize-and-play new-image size)))


;; Registering script

(script-fu-register
 "script-fu-resized-playback-with-onion-skinning"
 "Resized Playback with Onion Skinning"
 "Display before and after layers at once as a resized animation playback."
 "Burhanuddin Baharuddin"
 "Copyright 2019, Burhanuddin Baharuddin"
 "March 4, 2019"
 "RGB*"
 SF-IMAGE "Image" 0
 SF-DRAWABLE "Drawable" 0
 SF-OPTION "Size" '("1x" "2x" "4x" "8x" "16x")
 SF-OPTION "Mode" '("Back Frame" "Forward Frame" "Back & Forward Frames"))

(script-fu-menu-register "script-fu-resized-playback-with-onion-skinning"
                         "<Image>/Filters/Animation")
