;;; org-graph.el --- Graph the relationships between your Org-Mode files  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Daniel J. Rothblatt

;; Author: Daniel J. Rothblatt <djrothblatt@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org Graph crawls the links in a given Org-Mode file, producing a graph
;; of the Org-Mode files (and any other files/URLs it finds) and creating
;; some legible output.

;; Current output formats are PNG and SVG.

;;; Code:

(require 'cl-lib)
(require 'org)

(defgroup org-graph nil
  "Graph the relationships between your Org-Mode files"
  :group 'tools)

(defcustom org-graph-graphviz-command "dot"
  "The graphviz command to run."
  :group 'org-graph
  :type '(choice (const "dot")
                 (const "neato")
                 (const "circo")
                 (const "twopi")
                 (const "fdp")))

(defun org-graph--make-edge (source target)
  "Create link graph edge from SOURCE and TARGET."
  (list source target))

(defun org-graph--file-link->edge (link)
  "Turn file link LINK into graph edge."
  (org-graph--make-edge
   (buffer-file-name)
   (expand-file-name
    (org-element-property :path link))))

(defun org-graph--web-link->edge (link)
  "Turn web link LINK into graph edge."
  (org-graph--make-edge
   (buffer-file-name)
   (org-element-property :raw-link link)))

(defun org-graph--link->edge (link)
  "Create link graph edge from LINK."
  (cl-case (intern (org-element-property :type link))
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
  "Get graph edges from links in current buffer."
  (cl-remove-duplicates
   (org-element-map (org-element-parse-buffer) 'link
     #'org-graph--link->edge)
   :test #'equal))

(defun org-graph--vertices (graph)
  "Get the set of GRAPH's vertices."
  (cl-remove-duplicates
   (cl-loop for (source target) in graph
            collect source
            collect target)
   :test #'equal))

(defun org-graph--make-graph (buffer &optional visited-buffers)
  "Create graph from links in BUFFER, ignoring links in VISITED-BUFFERS.

First we collect all the links on the page, then we traverse the links that go to Org-Mode files."
  (with-current-buffer (find-file-noselect buffer)
    (let ((edges (org-graph--buffer-edges)))
      (cl-union edges
                (cl-loop for org-link in (org-graph--org-links (buffer-file-name) visited-buffers)
                         appending (org-graph--make-graph org-link
                                                          (cl-adjoin (buffer-file-name) visited-buffers)))
                :test #'equal))))

(defun org-graph--graph->graphviz (graph)
  "Create graphviz document as string from GRAPH.

GRAPH is an edge set ((source target) ...)."
  (format "digraph {\n%s\n%s}"
          (cl-loop for vertex in (org-graph--vertices graph)
                concat (format "  \"%s\" [URL=\"%s\"];\n" vertex vertex))

          (cl-loop for (source target) in graph
                concat (format "  \"%s\"->\"%s\";\n" source target))))

(cl-labels ((export (buffer file-name output-format)
                    (shell-command
                     (concat org-graph-graphviz-command
                             (format " -T %s -o %s.%s << EOF\n" output-format file-name output-format)
                             (org-graph--graph->graphviz
                              (org-graph--make-graph buffer))
                             "\nEOF")
                     "*org-graph*")))
  (defun org-graph/create-png (buffer)
   "Create graphviz document of BUFFER and display in other window."
   (interactive "bOrg buffer: ")
   (let ((file-name (gensym "org-graph-")))
     (export buffer file-name "png")
     (switch-to-buffer-other-window (find-file-noselect (concat "./" (symbol-name file-name) ".png")))))

 (defun org-graph/create-svg (buffer)
   "Create clickable svg graph of BUFFER and browse it."
   (interactive "bOrg buffer: ")
   (let ((file-name (gensym "org-graph-")))
     (export buffer file-name "svg")
     (browse-url-of-file
      (concat "file://" (expand-file-name (concat "./" (symbol-name file-name) ".svg")))))))

(provide 'org-graph)

;;; org-graph.el ends here
