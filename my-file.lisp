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
             (is equalp 1 (+ 1 0)))

#|
This file contains code that follows a Test-Driven Development (TDD) approach.
TDD steps:
1. Write a test that fails.
2. Write the minimum amount of code to make the test pass.
3. Refactor the code.
4. Repeat the process.
|#

#|
Lets implement a graph data structure and the Dijkstra's algorithm and the shortest path algorithm.

Nodes shall be Lisp Symbols.
Arrows shall be a structure with source, destination and weight.
Graph shall be a structure with nodes and arrows.

Besides the BOA constructors, we will also implement the following functions:
- Dijkstra's algorithm.
- Shortest path algorithm.

Dijkstra's algorithm pseudocode from Wikipedia with some modifications:
 1  function Dijkstra(Graph, source):
 2     
 3      for each vertex v in Graph.Vertices:
 4          distance[v] ← INFINITY
 5          previous[v] ← UNDEFINED
 6          add v to unvisited
 7      distance[source] ← 0
 8     
 9      while unvisited is not empty:
10          u ← vertex in unvisited with minimum distance[u]
11          remove u from unvisited
12         
13          for each neighbor v of u still in unvisited:
14              alt ← distance[u] + Graph.Edges(u, v) // Calculate the alternative distance from the source to neighbor v through u.
15              if alt < distance[v]:
16                  distance[v] ← alt
17                  previous[v] ← u
18
19      return distance[], previous[]


Distance and previous shall be hash tables.
We shall implement an auxiliary function to get the neighbors of a node.

|#

;;; Lets write the tests for the arrow structure.
(define-test (test-suite test-arrow-structure)
    (let ((arrow (make-arrow 'a 'b 1)))
      (is equalp 'a (arrow-source arrow))
      (is equalp 'b (arrow-destination arrow))
      (is equalp 1 (arrow-weight arrow))))

;;; Lets implement the arrow structure.
(defstruct (arrow (:constructor make-arrow (source destination weight)))
  source
  destination
  weight)

;;; Lets write the tests for the graph structure.
(define-test (test-suite test-graph-structure)
    (let ((graph (make-graph '(a b c) (list (make-arrow 'a 'b 1) (make-arrow 'b 'c 2)))))
      (is equalp '(a b c) (graph-nodes graph))
      (is equalp 2 (length (graph-arrows graph)))))

;;; Lets implement the graph structure.
(defstruct (graph (:constructor make-graph (nodes arrows)))
  nodes
  arrows)

;;; Lets write the tests for the get-neighbors function.
(define-test (test-suite test-get-neighbors)
    (let* ((graph (make-graph '(a b c) (list (make-arrow 'a 'b 1) (make-arrow 'b 'c 2))))
           (neighbors (get-neighbors 'a graph)))
      (is equalp 1 (length neighbors))
      (is equalp 'b (arrow-destination (first neighbors)))))

;;; Lets implement the get-neighbors function.
(defun get-neighbors (node graph)
  (remove-if-not (lambda (arrow) (eql node (arrow-source arrow))) (graph-arrows graph)))

;;; Lets write the tests for the Dijkstra's algorithm.
(define-test (test-suite test-dijkstra)
  ;; Lets mokup the graph (a -> b 1), (b -> c 2), (a -> c 4)
  (let ((graph (make-graph '(a b c) (list (make-arrow 'a 'b 1) (make-arrow 'b 'c 2) (make-arrow 'a 'c 4)))))
    (multiple-value-bind (distance previous) (dijkstra 'a graph)
      (is equalp 0 (gethash 'a distance))
      (is equalp 1 (gethash 'b distance))
      (is equalp 3 (gethash 'c distance))
      (is equalp 'a (gethash 'b previous))
      (is equalp 'b (gethash 'c previous)))))

;;; Lets implement the Dijkstra's algorithm.
(defun dijkstra (source graph)
  (let ((distance (make-hash-table))
        (previous (make-hash-table))
        (unvisited (copy-list (graph-nodes graph))))
    (dolist (node (graph-nodes graph))
      (setf (gethash node distance) most-positive-fixnum)
      (setf (gethash node previous) nil))
    (setf (gethash source distance) 0)
    (loop while unvisited
          do (let ((u (first (sort unvisited (lambda (a b) (< (gethash a distance) (gethash b distance)))))))
               (setf unvisited (remove u unvisited))
               (dolist (neighbor (get-neighbors u graph))
                 (let ((alt (+ (gethash u distance) (arrow-weight neighbor))))
                   (when (< alt (gethash (arrow-destination neighbor) distance))
                     (setf (gethash (arrow-destination neighbor) distance) alt)
                     (setf (gethash (arrow-destination neighbor) previous) u))))))
    (values distance previous)))
         
             
;;; Lets run the tests.  
(test '(test-suite))
