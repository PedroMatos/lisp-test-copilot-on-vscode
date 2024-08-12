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
             ;; Pattern: (is <test function> <expected result> <arguments>)
             (is equalp 1 (+ 0 1 0)))

#|
This file contains code that follows a Test-Driven Development (TDD) approach.
TDD basic steps:
1. Write a test that fails.
2. Write the minimum amount of code to make the test pass.
3. Refactor the code.
4. Repeat the process.
|#

#|
This this file we implement:
 * A graphs abstract data type using Lisp symbols as nodes and a simple data structure to represent the arrows.
 * BOA constructors to create a graph and an arrow.
 * The Dijkstra algorithm.
 * The shortest-path function using Dijkstra output.

 The graph has a list of nodes and a list of arrows. Each arrow has a source node, a target node, and a weight.

 The Dijkstra algorithm speudo-code from Wikipedia with some modifications and annotations:

 1  function Dijkstra(Graph, source):
 2     
 3      for each vertex v in Graph.Vertices:
 4          distance[v] ← INFINITY
 5          previous[v] ← UNDEFINED
 6          add v to Unvisited
 7      distance[source] ← 0
 8     
 9      while Unvisited is not empty:
10          u ← vertex in Unvisited with minimum distance[u]
11          remove u from Unvisited
12         
13          for each neighbor v of u still in Unvisited:
14              alt ← distance[u] + Graph.Edges(u, v)
15              if alt < distance[v]:
16                  distance[v] ← alt
17                  previous[v] ← u
18
19      return distance[], previous[]

Note:
* distance and previous are hash tables.

The graph abstract data type has the following operations:
 * (make-graph nodes arrows): Creates a graph with the given nodes and arrows.
 * (make-arrow source target weight): Creates an arrow with the given source, target, and weight.
 * (neighbors node graph): Returns the neighbors of the node in the graph.
 * (dijkstra graph source): Returns the distance and previous nodes of the graph using the Dijkstra algorithm.
 * (shortest-path source target graph): Returns the shortest path from the source to the target in the graph.
|#

;;; Let's start by writing the tests for the arrow constructor.
(define-test (test-suite test-arrow)
  (let ((arrow (make-arrow 'a 'b 1)))
    (is equalp 'a (arrow-source arrow))
    (is equalp 'b (arrow-target arrow))
    (is equalp 1 (arrow-weight arrow))))

;;; Let's implement the arrow BOA constructor.
(defstruct (arrow (:constructor make-arrow (source target weight)))
  source
  target
  weight)

;;; Let's write the tests for the graph constructor.
(define-test (test-suite test-graph)
  (let ((graph (make-graph '(a b) (list (make-arrow 'a 'b 1)))))
    (is equalp '(a b) (graph-nodes graph))
    (is equalp (list (make-arrow 'a 'b 1)) (graph-arrows graph))))

;;; Let's implement the graph BOA constructor.
(defstruct (graph (:constructor make-graph (nodes arrows)))
  nodes
  arrows)

;;; Let's write the tests for the neighbors function.
(define-test (test-suite test-neighbors)
  (let ((graph (make-graph '(a b c) (list (make-arrow 'a 'b 1) (make-arrow 'a 'c 2)))))
    (is equalp '(b c) (neighbors 'a graph))
    (is equalp '() (neighbors 'b graph))
    (is equalp '() (neighbors 'c graph))))

;;; Let's implement the neighbors function.
(defun neighbors (node graph)
  (mapcar #'arrow-target
          (remove-if-not (lambda (arrow)
                           (eq (arrow-source arrow) node))
                         (graph-arrows graph))))

;;; Let's write the tests for the Dijkstra algorithm.
(define-test (test-suite test-dijkstra)
  ;; Lets mokup the graph: (a -> b 1), (b -> c 2), (a -> c 4)
  (let ((graph (make-graph '(a b c)
                           (list (make-arrow 'a 'b 1)
                                 (make-arrow 'b 'c 2)
                                 (make-arrow 'a 'c 4)))))
    ;; disjkstra returns 2 values: distance and previous.
    (multiple-value-bind (distance previous)
        (dijkstra graph 'a)
      ;; distance and previous are hash tables.
      (is equalp 0 (gethash 'a distance))
      (is equalp 1 (gethash 'b distance))
      (is equalp 3 (gethash 'c distance))
      (is equalp 'a (gethash 'b previous))
      (is equalp 'b (gethash 'c previous)))))

;;; Let's implement the Dijkstra algorithm.
(defun dijkstra (graph source)
  (let ((distance (make-hash-table))
        (previous (make-hash-table)) ; nil is used as UNDEFINED
        (unvisited (copy-list (graph-nodes graph))))
    (dolist (node (graph-nodes graph))
      (setf (gethash node distance) most-positive-fixnum))
    (setf (gethash source distance) 0)
    (loop while unvisited
          do (let ((u (reduce (lambda (a b)
                                (if (< (gethash a distance) (gethash b distance))
                                    a
                                    b))
                              unvisited)))
               (setf unvisited (remove u unvisited))
               (mapc (lambda (v)
                       (let ((alt (+ (gethash u distance)
                                      (arrow-weight
                                        (find-if (lambda (arrow)
                                                  (and (eq (arrow-source arrow) u)
                                                        (eq (arrow-target arrow) v)))
                                                (graph-arrows graph)))))) ; Graph.Edges(u, v)
                         (when (< alt (gethash v distance))
                           (setf (gethash v distance) alt)
                           (setf (gethash v previous) u))))
                    (neighbors u graph))))
    (values distance previous)))

;;; The shortest-path function uses the Dijkstra output to return the shortest path and the distance.

;;; Let's write the tests for the shortest-path function.
(define-test (test-suite test-shortest-path)
  ;; Lets mokup the graph: (a -> b 1), (b -> c 2), (a -> c 4)
  (let ((graph (make-graph '(a b c)
                           (list (make-arrow 'a 'b 1)
                                 (make-arrow 'b 'c 2)
                                 (make-arrow 'a 'c 4)))))
    (multiple-value-bind (path distance)
        (shortest-path 'a 'c graph)
      (is equalp '(a b c) path)
      (is equalp 3 distance))))

;;; Let's implement the shortest-path function.
(defun shortest-path (source target graph)
  (multiple-value-bind (distance previous)
      (dijkstra graph source)
    (let ((path (list target))
          (current target))
      (loop while (not (eq current source))
            do (setf current (gethash current previous))
               (push current path))
      (values path (gethash target distance)))))

;;; Let's run the tests.
(test '(test-suite))
