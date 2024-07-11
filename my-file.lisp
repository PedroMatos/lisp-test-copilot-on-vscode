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
    (is eql 1 (+ 0 1 0)))

#|
This file contains code that follows a Test-Driven Development (TDD) approach.
|#

#|

Lets implement graphs, Dijkstra algorithm and the shortest path.

Dijkstra's algorithm pseudocode from wikipedia with some modifications:

 1  function Dijkstra(Graph, source):
 2     
 3      for each node vertex in Graph.nodes:
 4          distance[vertex] ← INFINITY
 5          previous[vertex] ← UNDEFINED
 6          add vertex to queue
 7      distance[source] ← 0
 8     
 9      while queue is not empty:
10          current ← node in queue with minimum distance[current]
11          remove current from queue
12         
13          for each neighbor neighbor of current still in queue:
14              alternative ← distance[current] + Graph.arrows(current, neighbor) ; Calculate the alternative distance by adding the distance from the current node to the neighbor node
15              if alternative < distance[neighbor]:
16                  distance[neighbor] ← alternative
17                  previous[neighbor] ← current
18
19      return distance[], previous[]

We shall implement the following abstract data types:

- Node - A node is a point in the graph. Shall be implemented by Lisp symbols.
- Arrow - An arrow is a connection between two nodes. Shall be implemented by a structure with a source, destination and weight.
- Graph - A graph is a collection of nodes and arrows. Shall be implemented by a structure with a list of nodes and a list of arrows.

We shall implement the following functions:

- neighbors - Given a node and a graph, returns a list of neighbors of the node.
- dijkstra - Given a graph and a source node, returns the distance and previous nodes for each node in the graph.
- shortest-path - Given a graph, a source node and a destination node, returns the shortest path between the source and destination nodes.

|#
    
(test '(test-suite))
