(define (script-fu-restructure-for-onion-skinning image layer bg-color)
  (let ((width (car (gimp-image-width image)))
        (height (car (gimp-image-height image)))
        (layers (reverse (vector->list (cadr (gimp-image-get-layers image)))))
        (prev-color (car (gimp-context-get-foreground))))

    (gimp-context-set-foreground bg-color)

    (gimp-progress-pulse)

    (gimp-image-undo-group-start image)

    (for-each
     (lambda (layer)
       (let ((new-group (car (gimp-layer-group-new image)))
             (background (car (gimp-layer-new image
                                              width
                                              height
                                              RGB-IMAGE
                                              "Background"
                                              100
                                              NORMAL-MODE))))

         (gimp-item-set-name new-group "Frame")

         (gimp-drawable-fill background FOREGROUND-FILL)

         (gimp-image-insert-layer image new-group 0 0)

         (gimp-image-insert-layer image background new-group 0)

         (gimp-image-reorder-item image layer new-group 0)))

     layers)

    (gimp-image-undo-group-end image)

    (gimp-progress-update 1)

    (gimp-context-set-foreground prev-color)

    (gimp-image-set-active-layer image layer)

    (gimp-displays-flush)))


;; Registering script

(script-fu-register
 "script-fu-restructure-for-onion-skinning"
 "Restructure for Onion Skinning"
 "Restructure layers to match the structure required by Onion Skinning script."
 "Burhanuddin Baharuddin"
 "Copyright 2019, Burhanuddin Baharuddin"
 "March 7, 2019"
 "RGB*"
 SF-IMAGE "Image" 0
 SF-DRAWABLE "Drawable" 0
 SF-COLOR "Background color" '(255 255 255))

(script-fu-menu-register "script-fu-restructure-for-onion-skinning"
                         "<Image>/Filters/Animation")
