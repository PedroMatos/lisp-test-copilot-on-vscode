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
             (is equalp 1 (+ 0 1 0)))

#|
This file contains code that follows a Test-Driven Development (TDD) approach.

Test-Driven Development (TDD) is a software development approach where tests are written before the actual code implementation. The development process in TDD typically follows these steps:
1. Write a failing test: Start by writing a test that describes the desired behavior of the code.
2. Run the test: Execute the test and ensure that it fails, indicating that the desired behavior is not yet implemented.
3. Write the code: Implement the code necessary to make the test pass.
4. Run the test again: Execute the test and verify that it now passes.
5. Refactor the code: Improve the code's design and structure while keeping the tests passing.
6. Repeat: Repeat the process for the next desired behavior or feature.

By following this iterative cycle of writing tests, implementing code, and refactoring, TDD aims to improve code quality, maintainability, and reliability.

|#

#|

Lets implement the shortest-path algorithm using Dijkstra's algorithm.

Dijkstra's algorithm is an algorithm for finding the shortest paths between nodes in a graph. It works by maintaining a set of nodes whose shortest distance from the source node is known. At each step, the algorithm selects the node with the smallest known distance and updates the distances of its neighbors. This process continues until all nodes have been visited.

Dijkstra's algorithm pseudocode from Wikipedia with annotations:

 1  function Dijkstra(Graph, source):
 2     
 3      for each node n in Graph.Nodes:
 4          distance[n] ← INFINITY
 5          previous[n] ← UNDEFINED
 6          add n to unvisitedNodes
 7      distance[source] ← 0
 8     
 9      while unvisitedNodes is not empty:
10          current ← node in unvisitedNodes with minimum distance[current]
11          remove current from unvisitedNodes
12         
13          for each neighbor n of current still in unvisitedNodes:
14              newDistance ← distance[current] + Graph.Arrows(current, n)
15              if newDistance < distance[n]:
16                  distance[n] ← newDistance
17                  previous[n] ← current
18
19      return distance[], previous[]

distance and previous shall be hash-tables where the key is the node and the value is the distance and the previous node respectively.

Abstract data types:
- Node: Nodes shall be Lisp symbols.
- Arrow: Arrows shall be structs with a source, destination, and weight.
- Graph: A graph data structure representing the nodes and arrows.

Auxiliary functions:
- Neighbors: A function that returns the neighbors of a given node.

Functions to implement:
- shortest-path: A function that calculates the shortest path between two nodes in a graph using Dijkstra's algorithm.
- dijkstra: The core Dijkstra's algorithm implementation.
|#

(define-test (test-suite test-arrow)
  (let ((arrow (make-arrow 'a 'b 5)))
    (is eq 'a (arrow-source arrow))
    (is eq 'b (arrow-destination arrow))
    (is eq 5 (arrow-weight arrow))
    (is equalp arrow (make-arrow 'a 'b 5))))

(defstruct (arrow (:constructor make-arrow (source destination weight)))
  source
  destination
  weight)

(define-test (test-suite test-graph)
  ;; Mock graph (a b 1) (b c 2)  (a c 4)
  (let ((graph (make-graph '(a b c) (list (make-arrow 'a 'b 1)
                                          (make-arrow 'b 'c 2)
                                          (make-arrow 'a 'c 4)))))
    (is equalp '(a b c) (graph-nodes graph) )
    (is equalp (list (make-arrow 'a 'b 1)
                     (make-arrow 'b 'c 2)
                     (make-arrow 'a 'c 4))
               (graph-arrows graph))))

(defstruct (graph (:constructor make-graph (nodes arrows)))
  nodes
  arrows)

(define-test (test-suite test-neighbors)
  ;; Mock graph (a b 1) (a b 2)  (a c 4)
  (let ((graph (make-graph '(a b c) (list (make-arrow 'a 'b 1)
                                          (make-arrow 'b 'c 2)
                                          (make-arrow 'a 'c 4)))))
    (is equalp '(b c) (neighbors graph 'a))
    (is equalp '(c) (neighbors graph 'b))
    (is equalp '() (neighbors graph 'c))))

(defun neighbors (graph node)
  (mapcar #'arrow-destination
          (remove-if-not (lambda (arrow)
                           (eq (arrow-source arrow) node))
                         (graph-arrows graph))))

(define-test (test-suite test-dijkstra)
  ;; Mock graph (a b 1) (b c 2)  (a c 4)
  (let ((graph (make-graph '(a b c) (list (make-arrow 'a 'b 1)
                                          (make-arrow 'b 'c 2)
                                          (make-arrow 'a 'c 4))))
        (source 'a))
    (multiple-value-bind (distance previous)
        (dijkstra graph source)
      (is equalp 0 (gethash source distance))
      (is equalp 1 (gethash 'b distance))
      (is equalp 3 (gethash 'c distance))
      (is equalp nil (gethash 'a previous))
      (is equalp 'a (gethash 'b previous))
      (is equalp 'b (gethash 'c previous)))))

(defun dijkstra (graph source)
  (let ((distance (make-hash-table))
        (previous (make-hash-table))
        (unvisited-nodes (copy-list (graph-nodes graph))))
    (dolist (node (graph-nodes graph))
      (setf (gethash node distance) (if (eq node source) 0 most-positive-fixnum)
            (gethash node previous) nil))
    (loop while unvisited-nodes
          do (let ((current (reduce (lambda (a b)
                                      (if (< (gethash a distance) (gethash b distance)) a b))
                                    unvisited-nodes)))
               (setf unvisited-nodes (remove current unvisited-nodes))
               (dolist (neighbor (neighbors graph current))
                 (let ((new-distance (+ (gethash current distance)
                                        (arrow-weight (find-if (lambda (arrow)
                                                                  (and (eq (arrow-source arrow) current)
                                                                       (eq (arrow-destination arrow) neighbor)))
                                                                (graph-arrows graph)))))
                       (current-distance (gethash neighbor distance)))
                   (when (< new-distance current-distance)
                     (setf (gethash neighbor distance) new-distance
                           (gethash neighbor previous) current))))))
    (values distance previous)))

(define-test (test-suite test-shortest-path)
  ;; Mock graph (a b 1) (b c 2)  (a c 4)
  (let ((graph (make-graph '(a b c) (list (make-arrow 'a 'b 1)
                                          (make-arrow 'b 'c 2)
                                          (make-arrow 'a 'c 4))))
        (source 'a)
        (destination 'c))
    (is equalp '(a b c) (shortest-path graph source destination))
    ;; shortest-path shall return (values path distance)
    (multiple-value-bind (path distance)
        (shortest-path graph source destination)
      (is equalp '(a b c) path)
      (is equalp 3 distance))))

(defun shortest-path (graph source destination)
  "Finds the shortest path in a graph from a source node to a destination node using Dijkstra's algorithm.

  Args:
    graph: A hash table representing the graph.
    source: The source node.
    destination: The destination node.

  Returns:
    A list representing the shortest path from the source node to the destination node.
    The distance of the shortest path from the source node to the destination node."
  (multiple-value-bind (distance previous)
      (dijkstra graph source)
    (let ((path '())
          (current destination))
      (loop while current
            do (push current path)
               (setf current (gethash current previous)))
      (values path (gethash destination distance)))))

(test '(test-suite))