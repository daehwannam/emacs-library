
;;; `pulseaudio-control'
;;; https://sr.ht/~flexibeast/pulseaudio-control/

(defhydra dhnam/pulseaudio-control ()
  "pulseaudio-control"
  ("q" nil "quit")

  ("+" pulseaudio-control-increase-sink-volume)
  ("=" pulseaudio-control-increase-sink-volume)
  ("-" pulseaudio-control-decrease-sink-volume)
  ("_" pulseaudio-control-decrease-sink-volume)

  ("n" pulseaudio-control-select-sink-by-name)
  ("v" pulseaudio-control-set-sink-volume))

(provide 'dhnam-pulseaudio-control)
