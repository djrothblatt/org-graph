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

(defun org-graph--org-links (buffer visited-buffers)
  "Get org links from BUFFER, excluding what's in VISITED-BUFFERS."
  (cl-labels ((org-file-p (file)
                         (string=
                          (file-name-extension file)
                          "org")))
    (with-current-buffer (find-file-noselect buffer)
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (link)
          (let ((path (expand-file-name (org-element-property :path link))))
            (when (org-file-p path)
              (unless (member path visited-buffers)
                (expand-file-name path)))))))))

(defun org-graph--buffer-edges ()
  (org-element-map (org-element-parse-buffer) 'link
    #'org-graph--link->edge))

(defun org-graph--make-graph (buffer &optional visited-buffers)
  "Create graph from links in BUFFER.

First we collect all the links on the page, then we traverse the links that go to Org-Mode files."
  (with-current-buffer (find-file-noselect buffer)
    (let ((edges (org-graph--buffer-edges)))
      (union edges
             (loop for org-link in (org-graph--org-links (buffer-file-name) visited-buffers)
                   appending (org-graph--make-graph org-link
                                                    (adjoin (buffer-file-name) visited-buffers)))
             :test #'equal))))

