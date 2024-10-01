;;; Lets start by loading the test library to write the tests.
;;; We will use the parachute library for this purpose.
;;; Homepage: https://github.com/Shinmera/parachute
(ql:quickload "parachute")

(defpackage :my-package
  (:use :cl :parachute))

(in-package :my-package)

;;; Check that parachute is available.

(define-test test-suite)

(define-test (test-suite test-parachute)
             :time-limit 0.2
             (sleep 0.1)
             (is equalp 1 (+ 0 1)))

#|
This file contains code that follows a Test-Driven Development (TDD) approach.
|#

#|
Lets implement a simple graph data structure with arrows, the Dijkstra algorithm, and the shortest path algorithm based on Dijkstra.
Arrows have a source, a destination, and a weight.

Follow the pseudocode from the wikipedia with some modifications:

 1  function Dijkstra(Graph, source):
 2     
 3      for each vertex v in Graph.Vertices:
 4          distance[v] ← INFINITY
 5          previous[v] ← UNDEFINED
 6          add v to unvisited-nodes
 7      distance[source] ← 0
 8     
 9      while unvisited-nodes is not empty:
10          u ← vertex in unvisited-nodes with minimum distance[u]
11          remove u from unvisited-nodes
12         
13          for each neighbor v of u still in unvisited-nodes:
14              alt ← distance[u] + Graph.Edges(u, v)
15              if alt < distance[v]:
16                  distance[v] ← alt
17                  previous[v] ← u
18
19      return distance[], previous[]

The algorithm returns the distance and the previous node for each node in the graph.
Distance and previous shall be hash tables.

The graph shall be represented as a list nodes and a list of arrows.
Nodes shall be symbols.

|#

;;; Lets start by testing the arrow data structure.
(define-test (test-suite test-arrow)
  (let ((arrow (make-arrow 'a 'b 1)))
    (is eql (arrow-source arrow) 'a)
    (is eql (arrow-destination arrow) 'b)
    (is eql (arrow-weight arrow) 1)))

;;; Lets implement the arrow data structure.
(defstruct (arrow (:constructor make-arrow (source destination weight)))
  source
  destination
  weight)

;;; Lets test the graph data structure.
(define-test (test-suite test-graph)
  (let ((graph (make-graph '(a b c) (list (make-arrow 'a 'b 1) (make-arrow 'b 'c 2)))))
    (is equalp (graph-nodes graph) '(a b c))
    (is equalp (graph-arrows graph) (list (make-arrow 'a 'b 1) (make-arrow 'b 'c 2)))))

;;; Lets implement the graph data structure.
(defstruct (graph (:constructor make-graph (nodes arrows)))
  nodes
  arrows)

;;; Lets test the Dijkstra algorithm.
(define-test (test-suite test-dijkstra)
  ;; Lets mock the graph (a b 1) (b c 2) (a c 4)
  (let ((graph (make-graph '(a b c) (list (make-arrow 'a 'b 1) (make-arrow 'b 'c 2) (make-arrow 'a 'c 4)))))
    (multiple-value-bind (distance previous) (dijkstra graph 'a)
      (is eql 0 (gethash 'a distance))
      (is eql 1 (gethash 'b distance))
      (is eql 3 (gethash 'c distance))
      (is eql nil (gethash 'a previous))
      (is eql 'a (gethash 'b previous))
      (is eql 'b (gethash 'c previous)))))

;;; Lets implement the Dijkstra algorithm.
(defun dijkstra (graph source)
  (let ((distance (make-hash-table :test 'eq))
        (previous (make-hash-table :test 'eq))
        (unvisited-nodes (copy-list (graph-nodes graph))))
    (dolist (node (graph-nodes graph))
      (setf (gethash node distance) most-positive-fixnum)
      (setf (gethash node previous) nil))
    (setf (gethash source distance) 0)
    (loop while unvisited-nodes
          do (let ((u (car (sort unvisited-nodes #'< :key (lambda (node) (gethash node distance))))))
               (setf unvisited-nodes (remove u unvisited-nodes))
               (dolist (arrow (remove-if-not (lambda (arrow) (eq (arrow-source arrow) u)) (graph-arrows graph)))
                 (let* ((v (arrow-destination arrow))
                        (alt (+ (gethash u distance) (arrow-weight arrow))))
                   (when (< alt (gethash v distance))
                     (setf (gethash v distance) alt)
                     (setf (gethash v previous) u))))))
    (values distance previous)))


(test '(test-suite))
