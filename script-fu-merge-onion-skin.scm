(define (script-fu-merge-onion-skin image)
  (let ((parasites (cadr (gimp-image-get-parasite-list image))))

    (if (not (member "onion-skinning-parent-id" parasites))
        (gimp-message
         "This image is not the image for onion skinning.")

        (let* ((parent-id (caddr
                           (car
                            (gimp-image-get-parasite
                             image
                             "onion-skinning-parent-id"))))
               (images (cadr (gimp-image-list)))
               (parent-image (foldr
                              (lambda (x y) (if (null? x) y x))
                              nil
                              (map
                               (lambda (image)
                                 (let ((parasites (cadr
                                                   (gimp-image-get-parasite-list
                                                    image))))
                                   (if (member "onion-skinning-id" parasites)
                                       (let ((id (caddr
                                                  (car
                                                   (gimp-image-get-parasite
                                                    image
                                                    "onion-skinning-id")))))
                                         (if (equal? id parent-id)
                                             image)))))
                               (vector->list images)))))

          (if (null? parent-image)
              (gimp-message
               "The image to which this onion skins belong to is not opened.")

              (let* ((top-frame (vector-ref
                                 (cadr (gimp-image-get-layers image)) 0))
                     (frame-name (car (gimp-item-get-name top-frame)))

                     (parent-frames (cadr (gimp-image-get-layers parent-image)))

                     (original-frame
                      (foldr
                       (lambda (x y) (if (null? x) y x))
                       nil
                       (map
                        (lambda (frame)
                          (if (equal? frame-name (car (gimp-item-get-name frame)))
                              frame))
                        (vector->list parent-frames))))

                     (original-position (car
                                         (gimp-image-get-item-position
                                          parent-image
                                          original-frame))))

                (let* ((new-frame (car
                                   (gimp-layer-new-from-drawable
                                    top-frame
                                    parent-image)))
                       (children (gimp-item-get-children new-frame))
                       (num-new-layers (car children))
                       (new-layers (cadr children))
                       (background (vector-ref new-layers (- num-new-layers 1))))

                  (gimp-item-set-name
                   new-frame
                   (car
                    (gimp-item-get-name top-frame)))

                  (gimp-item-set-visible background TRUE)

                  (gimp-image-remove-layer parent-image original-frame)

                  (gimp-image-insert-layer
                   parent-image
                   new-frame
                   0
                   original-position))))))))


;; Registering script

(script-fu-register
 "script-fu-merge-onion-skin"
 "Merge Onion Skin"
 "Merge back onion skin to previous image."
 "Burhanuddin Baharuddin"
 "Copyright 2019, Burhanuddin Baharuddin"
 "March 4, 2019"
 "RGB*"
 SF-IMAGE "Image" 0)

(script-fu-menu-register "script-fu-merge-onion-skin" "<Image>/Filters/Animation")
