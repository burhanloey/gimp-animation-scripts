(define (script-fu-merge-onion-skin image)

  (define +id+ "onion-skinning-id")
  (define +parent-id+ "onion-skinning-parent-id")

  (define (get-parasites image)
    (cadr (gimp-image-get-parasite-list image)))

  (define (get-parasite image name)
    (caddr
     (car
      (gimp-image-get-parasite image name))))

  (define (get-layers image)
    (cadr (gimp-image-get-layers image)))

  (define (find-if predicate alist)
    (foldr
     (lambda (x y) (if (null? x) y x))
     nil
     (map
      (lambda (item) (if (predicate item) item))
      alist)))

  (define (vector-first avector)
    (vector-ref avector 0))

  (define (vector-last avector)
    (vector-ref avector (- (vector-length avector) 1)))

  (define (get-name item)
    (car (gimp-item-get-name item)))



  (if (not (member +parent-id+ (get-parasites image)))
      (gimp-message
       "This image is not the image for onion skinning.")

      (let* ((parent-id (get-parasite image +parent-id+))
             (parent-image (find-if
                            (lambda (image)
                              (and
                               (member +id+ (get-parasites image))
                               (equal? (get-parasite image +id+) parent-id)))
                            (vector->list (cadr (gimp-image-list))))))

        (if (null? parent-image)
            (gimp-message
             (string-append
              "The image to which this onion skins belong to is not opened. "
              "If you open it again and it still does not work, you probably "
              "forgot to save and the reference metadata is gone."))

            (let* ((top-frame (vector-first (get-layers image)))
                   (top-frame-name (get-name top-frame))

                   (original-frame (find-if
                                    (lambda (frame)
                                      (equal? (get-name frame) top-frame-name))
                                    (vector->list (get-layers parent-image))))

                   (original-position (car
                                       (gimp-image-get-item-position
                                        parent-image
                                        original-frame)))

                   (new-frame (car
                               (gimp-layer-new-from-drawable
                                top-frame
                                parent-image)))

                   (background (vector-last
                                (cadr
                                 (gimp-item-get-children new-frame)))))

              (gimp-item-set-name new-frame top-frame-name)

              (gimp-item-set-visible background TRUE)

              (gimp-image-remove-layer parent-image original-frame)

              (gimp-image-insert-layer
               parent-image
               new-frame
               0
               original-position))))))


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
