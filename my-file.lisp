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
             (is eql 1 1))

#|
This file contains code that follows a Test-Driven Development (TDD) approach.

TDD Steps:
1. Write a failing test: Start by writing a test that describes the desired behavior of the code. This test should fail initially.
2. Write the minimum code: Write the minimum amount of code required to make the failing test pass. This code should be simple and straightforward.
3. Refactor the code: Once the test passes, refactor the code to improve its design, readability, and maintainability. Ensure that all tests still pass after refactoring.
4. Repeat: Repeat the above steps for each new feature or behavior you want to add to the code.

Note: The code in this file is not provided. It is recommended to follow the TDD approach when writing code to ensure better test coverage and maintainability.

|#

#|
Lets implement the Dijkstra's algorithm.
Dijkstra's algorithm pseudocode from Wikipedia with some annotations:

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
14              alt ← distance[u] + Graph.Edges(u, v)
15              if alt < distance[v]:
16                  distance[v] ← alt
17                  previous[v] ← u
18
19      return distance[], previous[]

distance is a map of vertices to their distances from the source vertex.
previous is a map of vertices to their previous vertex in the shortest path.
We shall implement them as hash tables in Common Lisp.

We shall use the following abstract data types:
- Graph: A graph data structure with nodes and arrows.
- Nodes: A Lisp symbols.
- Arrows: A structure with a source node, a target node, and a weight.

We shall implement the following functions:
- make-graph: Create a graph from a list of arrows. Examples:
  (make-graph) => #<GRAPH>
  (make-graph '(a b c) (list (make-arrow 'a 'b 1) (make-arrow 'b 'c 2))) => #<GRAPH>
- neighbors: Find the neighbors of a node in the graph. Examples:
  (neighbors (make-graph ...) 'a) => (list of nodes)
- dijkstra: Find the shortest path from a source node to all other nodes in the graph. Examples:
  (dijkstra (make-graph ...) 'a) => (values distance previous)
- shortest-path: Find the shortest path from a source node to a target node in the graph. Examples:
  (shortest-path (make-graph ...) 'a 'c) => (list of nodes)

|#

(define-test (test-suite test-arrow)
    (let ((arrow (make-arrow 'a 'b 1)))
      (is eql (arrow-source arrow) 'a)
      (is eql (arrow-target arrow) 'b)
      (is eql (arrow-weight arrow) 1)))

(defstruct (arrow (:constructor make-arrow (source target weight)))
  source
  target
  weight)

(define-test (test-suite test-graph)
    (let ((graph (make-graph '(a b c) (list (make-arrow 'a 'b 1) (make-arrow 'b 'c 2)))))
      (is equal (graph-nodes graph) '(a b c))
      (is equalp (graph-arrows graph) (list (make-arrow 'a 'b 1) (make-arrow 'b 'c 2)))))

(defstruct (graph (:constructor make-graph (nodes arrows)))
  nodes
  arrows)

(define-test (test-suite test-neighbors)
    (let* ((graph (make-graph '(a b c) (list (make-arrow 'a 'b 1) (make-arrow 'b 'c 2))))
           (neighbors-a (neighbors graph 'a))
           (neighbors-b (neighbors graph 'b))
           (neighbors-c (neighbors graph 'c)))
      (is equal neighbors-a '(b))
      (is equal neighbors-b '(c))
      (is equal neighbors-c nil))
    (let* ((graph (make-graph '(a b c) (list (make-arrow 'a 'b 1) (make-arrow 'b 'c 2) (make-arrow 'a 'c 3))))
           (neighbors-a (neighbors graph 'a))
           (neighbors-b (neighbors graph 'b))
           (neighbors-c (neighbors graph 'c)))
      (is equal neighbors-a '(b c))
      (is equal neighbors-b '(c))
      (is equal neighbors-c nil)))

(defun neighbors (graph node)
  (mapcar #'arrow-target (remove-if-not (lambda (arrow) (eql (arrow-source arrow) node)) (graph-arrows graph))))

(define-test (test-suite test-dijkstra)
    (let ((graph (make-graph '(a b c) (list (make-arrow 'a 'b 1) (make-arrow 'b 'c 2) (make-arrow 'a 'c 4)))))
      (multiple-value-bind (distance previous) (dijkstra graph 'a)
        (is equal (gethash 'a distance) 0)
        (is equal (gethash 'b distance) 1)
        (is equal (gethash 'c distance) 3)
        (is equal (gethash 'a previous) nil)
        (is equal (gethash 'b previous) 'a)
        (is equal (gethash 'c previous) 'b))))

(defun dijkstra (graph source)
  (let ((distance (make-hash-table :test #'eq))
        (previous (make-hash-table :test #'eq))
        (unvisited (copy-list (graph-nodes graph))))
    (dolist (node (graph-nodes graph))
      (setf (gethash node distance) (if (eql node source) 0 most-positive-fixnum)
            (gethash node previous) nil))
    (loop while unvisited do
          (let ((u (reduce (lambda (a b) (if (< (gethash a distance) (gethash b distance)) a b)) unvisited)))
            (setf unvisited (remove u unvisited))
            (dolist (v (neighbors graph u))
              (let ((alt (+ (gethash u distance) (arrow-weight (find-if (lambda (arrow) (and (equal (arrow-source arrow) u) (equal (arrow-target arrow) v))) (graph-arrows graph))))))
                (when (< alt (gethash v distance))
                  (setf (gethash v distance) alt)
                  (setf (gethash v previous) u))))))
    (values distance previous)))

(define-test (test-suite test-shortest-path)
    (let ((graph (make-graph '(a b c) (list (make-arrow 'a 'b 1) (make-arrow 'b 'c 2) (make-arrow 'a 'c 4)))))
      (multiple-value-bind (path-a-b distance-a-b) (shortest-path graph 'a 'b)
          (is equal '(a b) path-a-b)
          (is equal 1 distance-a-b))
      (multiple-value-bind (path-a-c distance-a-c) (shortest-path graph 'a 'c)
          (is equal '(a b c) path-a-c)
          (is equal 3 distance-a-c))
      (multiple-value-bind (path-b-c distance-b-c) (shortest-path graph 'b 'c)
          (is equal '(b c) path-b-c)
          (is equal 2 distance-b-c))))

(defun shortest-path (graph source target)
  ;; Return the shortest path from source to target using the previous hash table
  ;; and the distance hash table from the Dijkstra's algorithm.
  (multiple-value-bind (distance previous) (dijkstra graph source)
    (let ((path nil))
      (loop for node = target then (gethash node previous)
            while node
            do (push node path)
            finally (return (values path (gethash target distance)))))))
    
(test '(test-suite))
