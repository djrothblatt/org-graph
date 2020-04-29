(defun org-graph--make-edge (source target)
  "Creates link graph edge from SOURCE and TARGET."
  (list source target))

(defun org-graph--file-link->edge (link)
  "Turns file link LINK into graph edge."
  (org-graph--make-edge
   (buffer-file-name)
   (expand-file-name
    (org-element-property :path link))))

(defun org-graph--web-link->edge (link)
  "Turns web link LINK into graph edge."
  (org-graph--make-edge
   (buffer-file-name)
   (org-element-property :raw-link link)))

(defun org-graph--link->edge (link)
  "Create link graph edge from LINK."
  (case (intern (org-element-property :type link))
    (file (org-graph--file-link->edge link))
    ((http https) (org-graph--web-link->edge link))))
