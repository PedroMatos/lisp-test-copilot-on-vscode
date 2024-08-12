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

TDD main steps:
1. Write a test that fails.
2. Write the minimum amount of code to make the test pass.
3. Refactor the code.
4. Repeat.

|#

#|
Lets implement a graph data structure, the Dijkstra algorithm and a function to find the shortest path between two nodes.

Nodes shall be represented by Lisp symbols.
Arrows shall be a structure with source, destination and weight.
Graph shall be a structure with nodes and arrows.

Arrows and Graph shall have BOA constructors.

The Dijkstra algorithm pseudocode from Wikipedia with some modifications and annotations:

 1  function Dijkstra(graph, source):
 2     
 3      for each node node in graph.nodes:
 4          distance[node] ← INFINITY
 5          previous[node] ← UNDEFINED
 6          add node to queue  ; The queue is used to keep track of the nodes that have not been visited yet.
 7      distance[source] ← 0
 8     
 9      while queue is not empty:
10          current_node ← node in queue with minimum distance[current_node]
11          remove current_node from queue
12         
13          for each neighbor neighbor of current_node still in queue:
14              ; Calculate the alternative distance by adding the weight of the arrow from current_node to neighbor
15              alternative_distance ← distance[current_node] + graph.arrows(current_node, neighbor)
16              if alternative_distance < distance[neighbor]:
17                  distance[neighbor] ← alternative_distance
18                  previous[neighbor] ← current_node
19
20      return distance[], previous[]

Distance and previous shall be hash tables.
Dijkstra shall return 2 values: distance and previous.
Dijkstra shall use auxiliary functions to get the neighbors of a node.

|#

;;; Lets start by writing the tests for the arrow structure.
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

;;; Lets define the tests for the graph structure.
(define-test (test-suite test-graph-structure)
    (let ((graph (make-graph '(a b c) (list (make-arrow 'a 'b 1) (make-arrow 'b 'c 2)))))
      (is equalp '(a b c) (graph-nodes graph))
      (is equalp 2 (length (graph-arrows graph)))))

;;; Lets implement the graph structure.
(defstruct (graph (:constructor make-graph (nodes arrows)))
  nodes
  arrows)

;;; Lets test the auxiliary function to get the neighbors of a node.
(define-test (test-suite test-get-neighbors)
    (let ((graph (make-graph '(a b c) (list (make-arrow 'a 'b 1) (make-arrow 'b 'c 2)))))
      (is equalp '(b) (get-neighbors 'a graph))))

;;; Lets implement the auxiliary function to get the neighbors of a node.
(defun get-neighbors (node graph)
  (mapcar #'arrow-destination
          (remove-if-not (lambda (arrow)
                           (equalp node (arrow-source arrow)))
                         (graph-arrows graph))))

;;; Lets test the Dijkstra algorithm.
(define-test (test-suite test-dijkstra)
    ;; Lets mokup the graph (a -> b 1) (b -> c 2) (a -> c 4)
    (let ((graph (make-graph '(a b c) (list (make-arrow 'a 'b 1) (make-arrow 'b 'c 2) (make-arrow 'a 'c 4)))))
      (multiple-value-bind (distance previous) (dijkstra 'a graph)
        (is equalp 0 (gethash 'a distance))
        (is equalp 1 (gethash 'b distance))
        (is equalp 3 (gethash 'c distance))
        (is equalp 'a (gethash 'b previous))
        (is equalp 'b (gethash 'c previous)))))

;;; Lets implement the Dijkstra algorithm.
(defun dijkstra (source graph)
  (let ((distance (make-hash-table))
        (previous (make-hash-table))
        (queue (copy-list (graph-nodes graph))))
    (dolist (node (graph-nodes graph))
      (setf (gethash node distance) most-positive-fixnum
            (gethash node previous) nil))
    (setf (gethash source distance) 0)
    (loop while queue
          do (let ((current-node (first (sort queue (lambda (a b)
                                                       (< (gethash a distance) (gethash b distance)))))))
               (setf queue (remove current-node queue))
               (dolist (neighbor (get-neighbors current-node graph))
                 (let ((alternative-distance (+ (gethash current-node distance) (arrow-weight (find-if (lambda (arrow)
                                                                                                                (and (equalp (arrow-source arrow) current-node)
                                                                                                                      (equalp (arrow-destination arrow) neighbor)))
                                                                                                       (graph-arrows graph))))))
                   (when (< alternative-distance (gethash neighbor distance))
                     (setf (gethash neighbor distance) alternative-distance
                           (gethash neighbor previous) current-node))))))
    (values distance previous)))

;;; Lets define the tests for the function to find the shortest path between two nodes and its weight.
(define-test (test-suite test-shortest-path)
    ;; Lets mokup the graph (a -> b 1) (b -> c 2) (a -> c 4)
    (let ((graph (make-graph '(a b c) (list (make-arrow 'a 'b 1) (make-arrow 'b 'c 2) (make-arrow 'a 'c 4)))))
      (multiple-value-bind (path weight) (shortest-path 'a 'c graph)
        (is equalp '(a b c) path)
        (is equalp 3 weight)))
    ;; Lets mokup the graph (a -> b 1) (b -> c 2) (a -> c 1)
    (let ((graph (make-graph '(a b c) (list (make-arrow 'a 'b 1) (make-arrow 'b 'c 2) (make-arrow 'a 'c 1)))))
      (multiple-value-bind (path weight) (shortest-path 'a 'c graph)
        (is equalp '(a c) path)
        (is equalp 1 weight))))


;;; Lets implement the function to find the shortest path between two nodes and its weight.
(defun shortest-path (source destination graph)
  (multiple-value-bind (distance previous) (dijkstra source graph)
    (let ((path (list destination))
          (current-node destination))
      (loop while (not (equalp source current-node))
            do (setf current-node (gethash current-node previous)
                     path (cons current-node path)))
      (values path (gethash destination distance)))))


(test '(test-suite))
