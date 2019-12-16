;; Uppercase first letter keeping remaining string casing intact
(defun upcase-first (str)
  (concat (upcase (seq-take str 1))
          (seq-drop str 1)))

;; Java getter generation functions
(defun gen-getter-name (var-name)
  (concat "get" (upcase-first var-name)))

(defun gen-getter (type-str var-name)
  (let ((getter-name (gen-getter-name var-name)))
    (format "public %s %s() {\n\t return %s;\n}" type-str getter-name var-name)))

;; Java setter generation functions
(defun gen-setter-name (var-name)
  (concat "set" (upcase-first var-name)))

(defun gen-setter (type-str var-name)
  (let ((setter-name (gen-setter-name var-name)))
    (format "public void %s(%s %s) {\n\t this.%s = %s;\n}"
            setter-name
            type-str
            var-name
            var-name
            var-name)))

(defun gen-get-and-set (line)
  (let ((parts (last (split-string line) 2)))
    (let ((type (car parts))
          (var-name (substring (cadr parts) 0 -1))) ;; need to strip ';'
      (progn
        (insert "\n")
        (insert (gen-getter type var-name))
        (insert "\n\n")
        (insert (gen-setter type var-name))
        (insert "\n")))))

(defun insert-single-get-and-set (line)
  (interactive (list (format "%s" (thing-at-point 'line))))
  (progn
    (end-of-line)
    (gen-get-and-set line)))

(defun insert-get-and-set-multiple (region)
  (interactive (list (format "%s" (buffer-substring (region-beginning) (region-end)))))
  (let ((parts (split-string region "\n")))
    (dolist (line parts)
      (gen-get-and-set line))))

(provide 'java-getter-setter)
