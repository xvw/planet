(defvar planet-path "/home/xvw/Programmation/planet")

(defun planet-path ()
  "Show the planet path"
  (interactive)
  (message planet-path))

(defun planet-build (subject)
  "Build planet artifacts"
  (interactive "sSubject: ")
  (with-output-to-temp-buffer "*planet-build"
    (shell-command
     (format "(cd %s; ./build.exe %s)" planet-path subject)
     "*planet-build")
    (pop-to-buffer "*planet-build")))

(defun planet-location ()
  "Show location list"
  (interactive)
  (with-output-to-temp-buffer "*planet-build"
    (shell-command
     (format "(cd %s; cat logs/whereami.qube)" planet-path)
     "*planet-build")
    (pop-to-buffer "*planet-build")))

(defun planet-whereami (city country)
  "Track current Location"
  (interactive "sCountry: \nsCity: ")
  (with-output-to-temp-buffer "*planet-build"
    (shell-command
     (format "(cd %s; ./log.exe w --city %s --country %s)"
             planet-path
             city
             country)
     "*planet-build")
    (pop-to-buffer "*planet-build")))

(defun planet-log (duration sector project task)
  "Track current task"
  (interactive "nDuration: \nsSector: \nsProject: \nsLabel: ")
  (with-output-to-temp-buffer "*planet-build"
    (shell-command
     (format "(cd %s; ./log.exe r --duration %d --sector %s --project %s \"%s\")"
             planet-path
             duration
             sector
             project
             task)
     "*planet-build")
    (pop-to-buffer "*planet-build")))
