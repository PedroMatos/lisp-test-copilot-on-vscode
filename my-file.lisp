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
|#

#|
Lets implement a simple graph data structure and the shortest path algorithm based on the Dijkstra algorithm.

The graph will be represented as a list of nodes and a list of arrows.
Nodes shall be represented by Lisp symbols.
Arrows shall be represented by structure of three elements: the source node, the target node, and the weight of the arrow.

Dijkstra algorithm pseudocode from Wikipedia with some modifications:
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
13          for each arrow a from u still in unvisited-nodes:
14              alt ← distance[u] + Graph.Weight(a)  ; Calculate the alternative path distance to neighbor v through u
15              if alt < distance[target(a)]:
16                  distance[target(a)] ← alt
17                  previous[target(a)] ← u
18
19      return distance[], previous[]

Function Dijkstra returns 2 values: the distance from the source node to each node in the graph and the previous node in the shortest path from the source node to each node in the graph. Both values are represented as a hash table.

|#

;;; Lets test the arrow structure.
(define-test (test-suite test-arrow)
  (let ((arrow (make-arrow 'a 'b 1)))
    (is eql (arrow-source arrow) 'a)
    (is eql (arrow-target arrow) 'b)
    (is eql (arrow-weight arrow) 1)))

;;; Lets implement the arrow structure.
(defstruct (arrow (:constructor make-arrow (source target weight)))
  source
  target
  weight)

;;; Lets test the graph structure.
(define-test (test-suite test-graph)
  (let ((graph (make-graph '(a b c) (list (make-arrow 'a 'b 1) (make-arrow 'b 'c 2) (make-arrow 'c 'a 4)))))
    (is equalp (graph-nodes graph) '(a b c))
    (is equalp (graph-arrows graph) (list (make-arrow 'a 'b 1) (make-arrow 'b 'c 2) (make-arrow 'c 'a 4)))))

;;; Lets implement the graph structure.
(defstruct (graph (:constructor make-graph (nodes arrows)))
  nodes
  arrows)

;;; Lets test the Dijkstra algorithm.
(define-test (test-suite test-dijkstra)
  ;; Lets mock the graph (a b c) with arrows (a b 1) (b c 2) (a c 4).
  (let ((graph (make-graph '(a b c) (list (make-arrow 'a 'b 1) (make-arrow 'b 'c 2) (make-arrow 'a 'c 4)))))
    (multiple-value-bind (distance previous) (dijkstra graph 'a)
      (is equalp (gethash 'a distance) 0)
      (is equalp (gethash 'b distance) 1)
      (is equalp (gethash 'c distance) 3)
      (is equalp (gethash 'a previous) nil)
      (is equalp (gethash 'b previous) 'a)
      (is equalp (gethash 'c previous) 'b))))

;;; Helper function to find the node with the smallest distance
(defun find-min-distance-node (nodes distance)
  (reduce (lambda (a b)
            (if (< (gethash a distance) (gethash b distance)) a b))
          nodes))

;;; Lets implement the Dijkstra algorithm.
(defun dijkstra (graph source)
  (let ((distance (make-hash-table :test 'eq))
        (previous (make-hash-table :test 'eq))
        (unvisited-nodes (copy-list (graph-nodes graph))))
    
    ;; Initialize distances and previous nodes
    (dolist (node (graph-nodes graph))
      (setf (gethash node distance) most-positive-fixnum)
      (setf (gethash node previous) nil))
    (setf (gethash source distance) 0)
    
    ;; Main loop
    (loop while unvisited-nodes do
          (let ((u (find-min-distance-node unvisited-nodes distance)))
            (setf unvisited-nodes (remove u unvisited-nodes))
            (dolist (arrow (graph-arrows graph))
              (when (and (eql (arrow-source arrow) u)
                         (member (arrow-target arrow) unvisited-nodes))
                (let ((alt (+ (gethash u distance) (arrow-weight arrow))))
                  (when (< alt (gethash (arrow-target arrow) distance))
                    (setf (gethash (arrow-target arrow) distance) alt)
                    (setf (gethash (arrow-target arrow) previous) u)))))))
    
    (values distance previous)))

;;; Lets implement the Shortest Path algorithm using the Dijkstra algorithm. Shortest Path returns 2 values: the shortest path from the source node to the target node and the distance of the shortest path.
;;; Lets test the Shortest Path algorithm.
(define-test (test-suite test-shortest-path)
  ;; Lets mock the graph (a b c) with arrows (a b 1) (b c 2) (a c 4).
  (let ((graph (make-graph '(a b c) (list (make-arrow 'a 'b 1) (make-arrow 'b 'c 2) (make-arrow 'a 'c 4)))))
    (multiple-value-bind (path distance) (shortest-path graph 'a 'c)
      (is equalp '(a b c) path)
      (is equalp distance 3))))

;;; Lets implement the Shortest Path algorithm.
(defun shortest-path (graph source target)
  (multiple-value-bind (distance previous) (dijkstra graph source)
    (let ((path (list target))
          (node target))
      (loop while (gethash node previous)
            do
            (setf node (gethash node previous))
            (push node path))
      (values path (gethash target distance)))))

#|
Time ADT

The time abstract data type (ADT) shall represent time as an integer number of seconds.

The time ADT shall support the following operations:
- make.time: creates a time ADT from hours, minutes, and seconds.
- time.hours: returns the hours of the time ADT.
- time.minutes: returns the minutes of the time ADT.
- time.seconds: returns the seconds of the time ADT.
- time.to.string: returns the string representation of the time ADT.
- time.+ : adds two time ADTs.
- time.- : subtracts two time ADTs.
- time.< : compares two time ADTs.
- time.> : compares two time ADTs.
- time.<= : compares two time ADTs.
- time.>= : compares two time ADTs.
- time.= : compares two time ADTs.

|#

;;; Lets test the time ADT.
(define-test (test-suite test-time)
  (let ((time1 (make.time 1 2 3))
        (time2 (make.time 4 5 6)))
    (is equalp 1 (time.hours time1))
    (is equalp 2 (time.minutes time1))
    (is equalp 3 (time.seconds time1))
    (is equalp "01:02:03" (time.to.string time1))
    (is equalp (make.time 5 7 9) (time.+ time1 time2))
    (is equalp (make.time 3 3 3) (time.- time2 time1))
    (is eql t (time.< time1 time2))
    (is eql nil (time.> time1 time2))
    (is eql t (time.<= time1 time2))
    (is eql nil (time.>= time1 time2))
    (is eql nil (time.= time1 time2))))

;;; Lets implement the time ADT.
(defun make.time (hours minutes seconds)
  (+ (* hours (* 60 60)) (* minutes 60) seconds))

(defun time.hours (time)
  (floor (/ time (* 60 60))))

(defun time.minutes (time)
  (floor (/ (mod time (* 60 60)) 60)))

(defun time.seconds (time)
  (mod time 60))

(defun time.to.string (time)
  (format nil "~2,'0D:~2,'0D:~2,'0D" (time.hours time) (time.minutes time) (time.seconds time)))

(defun time.+ (time1 time2)
  (+ time1 time2))

(defun time.- (time1 time2)
  (- time1 time2))

(defun time.< (time1 time2)
  (< time1 time2))

(defun time.> (time1 time2)
  (> time1 time2))

(defun time.<= (time1 time2)
  (<= time1 time2))

(defun time.>= (time1 time2)
  (>= time1 time2))

(defun time.= (time1 time2)
  (= time1 time2))

;;; Lets run the tests.
(test '(test-suite))
