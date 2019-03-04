(define (script-fu-resized-playback image size)
  "Main entry."
  (let* (
         ;; Determines scale from size(index). scale = 2 ^ index
         (scale (expt 2 size))

         ;; Get width and height
         (width (car (gimp-image-width image)))
         (height (car (gimp-image-height image)))

         ;; Scale width and height
         (scaled-width (* width scale))
         (scaled-height (* height scale))

         ;; Duplicate image
         (new-image (car (gimp-image-duplicate image)))

         ;; Create empty layer for animation playback
         (layer (car (gimp-layer-new new-image
                                     scaled-width scaled-height
                                     RGB-IMAGE "Layer 1" 100 NORMAL-MODE))))

    ;; Set interpolation to none to avoid blur
    (gimp-context-set-interpolation INTERPOLATION-NONE)

    ;; Scale image
    (gimp-image-scale new-image scaled-width scaled-height)

    ;; Update progress bar
    (gimp-progress-update 1.0)

    ;; Run animation playback
    (plug-in-animationplay RUN-INTERACTIVE new-image layer)

    ;; Return scaled image for other scripts
    (list new-image)))


;; Registering script

(script-fu-register
 "script-fu-resized-playback"
 "Resized Playback"
 "Resize image before animation playback."
 "Burhanuddin Baharuddin"
 "Copyright 2019, Burhanuddin Baharuddin"
 "March 2, 2019"
 "RGB*"
 SF-IMAGE "Image" 0
 SF-OPTION "Size" '("1x" "2x" "4x" "8x" "16x"))

(script-fu-menu-register "script-fu-resized-playback" "<Image>/Filters/Animation")
