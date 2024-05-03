#|

This file implements a simple graph and then some algorithms to traverse it like the shortest path algorithm (Dijkstra). 

Although we follow a TDD the tests are not written in the TDD style. We first write the tests and then the implementation to avoid compilation warnings.

|#

;;; Lets start by loading the test library to write the tests.
;;; (ql:quickload "parachute")

(defpackage :graph
  (:use :cl :parachute))

(in-package :graph)

;;; Lets implement a non-negative weighted directed graph. A graph is a collection of nodes and arrows between them. Each arrow has a weight associated with it.
;;; We'll use Lisp symbols as nodes.
;;; Arrows are represented as a structures with a source, destination and weight.
(defstruct (arrow (:constructor make-arrow (source destination weight)))
  source
  destination
  weight)

(defstruct (graph (:constructor make-graph (nodes arrows)))
  nodes
  arrows)

;;; Lets test the graph implementation constructors.

(define-test graph-test-suite)

(define-test test-arrow-constructors
    :parent graph-test-suite
    (let ((source 'a)
          (destination 'b)
          (weight 1))
        (is equalp (make-arrow source destination weight) (make-arrow source destination weight))
        (is eq source (arrow-source (make-arrow source destination weight)))
        (is eq destination (arrow-destination (make-arrow source destination weight)))
        (is eq weight (arrow-weight (make-arrow source destination weight)))))

(define-test test-graph-constructors
    :parent graph-test-suite
    (let ((nodes '(a b c))
          (arrows (list (make-arrow 'a 'b 1)
                        (make-arrow 'b 'c 2))))
        (is equalp (make-graph nodes arrows) (make-graph nodes arrows))
        (is equalp nodes (graph-nodes (make-graph nodes arrows)))
        (is equalp arrows (graph-arrows (make-graph nodes arrows)))))

;;; Lets implement a function to get the neighbors of a node in a graph.
(defun neighbors (graph node)
    (mapcan (lambda (arrow)
                (if (eq node (arrow-source arrow))
                    (list (arrow-destination arrow))
                    nil))
        (graph-arrows graph)))

(define-test test-neighbors
    :parent graph-test-suite
    (let ((nodes '(a b c))
          (arrows (list (make-arrow 'a 'b 1)
                        (make-arrow 'b 'c 2))))
        (let ((graph (make-graph nodes arrows)))
            (is equalp '(b) (neighbors graph 'a))
            (is equalp '(c) (neighbors graph 'b))
            (is equalp nil (neighbors graph 'c)))))

(define-test test-neighbors-2
    :parent graph-test-suite
    (let ((nodes '(a b c))
          (arrows (list (make-arrow 'a 'b 1)
                        (make-arrow 'a 'c 2)
                        (make-arrow 'b 'c 3))))
      (let ((graph (make-graph nodes arrows)))
        (is equalp '(b c) (neighbors graph 'a))
        (is equalp '(c) (neighbors graph 'b))
        (is equalp nil (neighbors graph 'c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
Simplified version of Dijkstra's algorithm in pseudo code:
1. function Dijkstra(Graph, source):
2.     create vertex set Unvisited
3.     
4.     for each vertex v in Graph:             
5.         distance[v] := INFINITY                 // Initialize all distances to infinity
6.         previous[v] := UNDEFINED                // Track the previous node in optimal path
7.         add v to Unvisited                      // All nodes are initially unvisited
8.     distance[source] := 0                       // Distance from source to itself is zero
9.     
10.    while Unvisited is not empty:
11.        u := vertex in Unvisited with minimum distance[u]  // Select vertex with smallest distance
12.        remove u from Unvisited                            // Mark as visited
13.        
14.        for each neighbor v of u:                          // Explore each neighbor of u
15.            alternativePath := distance[u] + length(u, v)
16.            if alternativePath < distance[v]:               // A shorter path has been found
17.                distance[v] := alternativePath 
18.                previous[v] := u                            // Update previous node
19.                
20.    return distance, previous
distance is a hash table with the distance from the source to each node.
previous is a hash table with the previous node in the optimal path to each node.
|#
(defun dijkstra (graph source)
    (let ((unvisited (copy-list (graph-nodes graph)))
            (distance (make-hash-table))
            (previous (make-hash-table)))
        (dolist (node (graph-nodes graph))
        (setf (gethash node distance) most-positive-fixnum)
        (setf (gethash node previous) nil)
        (push node unvisited))
        (setf (gethash source distance) 0)
        (loop while unvisited
            do (let ((u (reduce (lambda (a b)
                                    (if (< (gethash a distance) (gethash b distance))
                                        a
                                        b))
                                unvisited)))
                (setf unvisited (remove u unvisited))
                (dolist (v (neighbors graph u))
                    (let ((alternative-path (+ (gethash u distance) (arrow-weight (find-if (lambda (arrow)
                                                                                                (and (eq u (arrow-source arrow))
                                                                                                    (eq v (arrow-destination arrow))))
                                                                                            (graph-arrows graph)))))
                        (current-distance (gethash v distance)))
                    (if (< alternative-path current-distance)
                        (progn
                            (setf (gethash v distance) alternative-path)
                            (setf (gethash v previous) u)))))))
    (values distance previous)))

(define-test test-dijkstra
  :parent graph-test-suite
  (let* ((nodes '(a b c d e))
         (arrows (list (make-arrow 'a 'b 1)
                       (make-arrow 'a 'c 2)
                       (make-arrow 'b 'c 3)
                       (make-arrow 'b 'd 4)
                       (make-arrow 'c 'd 5)
                       (make-arrow 'd 'e 6)))
         (graph (make-graph nodes arrows)))
    (multiple-value-bind (distance previous) (dijkstra graph 'a)
    (is equalp 0 (gethash 'a distance))
    (is equalp 1 (gethash 'b distance))
    (is equalp 2 (gethash 'c distance))
    (is equalp 5 (gethash 'd distance))
    (is equalp 11 (gethash 'e distance))
    (is equalp nil (gethash 'a previous))
    (is equalp 'a (gethash 'b previous))
    (is equalp 'a (gethash 'c previous))
    (is equalp 'b (gethash 'd previous))
    (is equalp 'd (gethash 'e previous)))))

;;; Run the test suite.
(parachute:test 'graph-test-suite :report 'summary)
