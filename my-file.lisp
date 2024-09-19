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
|#

#|
Lets implement simple graph model and the shortest path algorithm based on Dijkstra's algorithm.

The graph model will be represented as a list of nodes and arrows.
Each node will be represented a symbol.
Arrow will be represented as a structure with source, destination and weight.

Follows the Dijkstra's algorithm pseudo code from wikipedia with some annotations:
 1  function Dijkstra(Graph, source):
 2     
 3      for each node n in Graph.Nodes:
 4          distance[n] ← INFINITY
 5          previous[n] ← UNDEFINED
 6          add n to unvisited-nodes
 7      distance[source] ← 0
 8     
 9      while unvisited-nodes is not empty:
10          u ← node in unvisited-nodes with minimum distance[u]
11          remove u from unvisited-nodes
12         
13          for each neighbor v of u still in unvisited-nodes:
14              alt ← distance[u] + Graph.Arrows(u, v) ; Calculate the alternative path distance to neighbor v through u
15              if alt < distance[v]: ; If the calculated alternative path distance is shorter than the known distance to v
16                  distance[v] ← alt
17                  previous[v] ← u
18
19      return distance[], previous[]

Distance and previous will be represented as hash-tables.

|#

;;; Lets start by writing the tests for the arrow structure.
(define-test (test-suite test-arrow)
  (let ((arrow (make-arrow 'a 'b 1)))
    (is equalp 'a (arrow-source arrow))
    (is equalp 'b (arrow-destination arrow))
    (is equalp 1 (arrow-weight arrow))))

;;; Lets implement the arrow structure.
(defstruct (arrow (:constructor make-arrow (source destination weight)))
  source
  destination
  weight)

;;; Lets test the graph model.
(define-test (test-suite test-graph)
  (let ((graph (make-graph (list 'a 'b) (list (make-arrow 'a 'b 1)))))
    (is equalp (list 'a 'b) (graph-nodes graph))
    (is equalp (list (make-arrow 'a 'b 1)) (graph-arrows graph))))

;;; Lets implement the graph model.
(defstruct (graph (:constructor make-graph (nodes arrows)))
  nodes
  arrows)

;;; Lets test the Dijkstra's algorithm.
(define-test (test-suite test-dijkstra)
  (let ((graph (make-graph (list 'a 'b 'c)
                           (list (make-arrow 'a 'b 1)
                                 (make-arrow 'b 'c 2)
                                 (make-arrow 'a 'c 4)))))
    (multiple-value-bind (distance previous)
        (dijkstra graph 'a)
      (is equalp 1 (gethash 'b distance))
      (is equalp 3 (gethash 'c distance))
      (is equalp 'a (gethash 'b previous))
      (is equalp 'b (gethash 'c previous)))))

;;; Lets implement the Dijkstra's algorithm.
(defun dijkstra (graph source)
  (let ((distance (make-hash-table))
        (previous (make-hash-table))
        (unvisited-nodes (copy-list (graph-nodes graph))))
    (dolist (node (graph-nodes graph))
      (setf (gethash node distance) most-positive-fixnum)
      (setf (gethash node previous) nil))
    (setf (gethash source distance) 0)
    (loop while unvisited-nodes
          do (let ((u (reduce (lambda (a b)
                                (if (< (gethash a distance) (gethash b distance))
                                    a
                                    b))
                              unvisited-nodes)))
               (setf unvisited-nodes (remove u unvisited-nodes))
               (dolist (v (remove u unvisited-nodes))
                 (let ((alt (+ (gethash u distance) (arrow-weight (find-arrow graph u v)))))
                   (when (< alt (gethash v distance))
                     (setf (gethash v distance) alt)
                     (setf (gethash v previous) u))))))
    (values distance previous)))

;;; Test for the find-arrow function
(define-test test-find-arrow
  (let ((graph (make-graph (list 'a 'b 'c)
                           (list (make-arrow 'a 'b 1)
                                 (make-arrow 'b 'c 2)
                                 (make-arrow 'a 'c 4)))))
    (is equalp nil (find-arrow graph 'c 'a))
    (is equalp (make-arrow 'a 'b 1) (find-arrow graph 'a 'b))
    (is equalp (make-arrow 'b 'c 2) (find-arrow graph 'b 'c))))

(defun find-arrow (graph source destination)
  (find-if (lambda (arrow)
             (and (eq (arrow-source arrow) source)
                  (eq (arrow-destination arrow) destination)))
           (graph-arrows graph)))

;;; Lets test the shortest-path function.
(define-test (test-suite test-shortest-path)
  (let ((graph (make-graph (list 'a 'b 'c)
                           (list (make-arrow 'a 'b 1)
                                 (make-arrow 'b 'c 2)
                                 (make-arrow 'a 'c 4)))))
        ;; shortest-path returns 2 values: path and distance
    (multiple-value-bind (path distance)
        (shortest-path graph 'a 'c)
      (is equalp (list 'a 'b 'c) path)
      (is equalp 3 distance))))

;;; Lets implement the shortest-path function.
(defun shortest-path (graph source destination)
  (multiple-value-bind (distance previous)
      (dijkstra graph source)
    (let ((path nil))
      (loop for node = destination then (gethash node previous)
            while node
            do (push node path))
      (values path (gethash destination distance)))))

(test '(test-suite))
