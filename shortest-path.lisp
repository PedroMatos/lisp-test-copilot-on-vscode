#|
An implementation of the shortest path algorithm using Dijkstra's algorithm.

In this implementation we fallow a TDD approach to implement the algorithm. So, we start by writing the tests and then we implement the algorithm.

The algorithm is implemented using a priority queue to store the vertices that are being processed.

The algorithm is implemented in the function `shortest-path` that receives a graph, a start vertex and an end vertex and returns the shortest path between the start and end vertices.

The graph is represented as a map where the keys are the vertices and the values are maps where the keys are the neighbors of the vertex and the values are the weights of the edges that connect the vertex to its neighbors.

The priority queue is implemented as a map where the keys are the vertices and the values are the distances of the vertices to the start vertex.

The algorithm works as follows:

1. Initialize the priority queue with the start vertex with distance 0 and all other vertices with distance infinity.
2. While the priority queue is not empty:
    1. Get the vertex with the smallest distance from the priority queue.
    2. If the vertex is the end vertex, return the distance to the vertex.
    3. For each neighbor of the vertex:
        1. Calculate the distance to the neighbor.
        2. If the distance is smaller than the current distance to the neighbor, update the distance to the neighbor in the priority queue.
3. If the end vertex was not reached, return nil.

The algorithm has a time complexity of O((V + E) log V) where V is the number of vertices and E is the number of edges in the graph.

The algorithm has a space complexity of O(V) where V is the number of vertices in the graph.

|#

;;; Lets start by loading the test library to write the tests.
(ql:quickload "parachute")

(defpackage :dijkstra
  (:use :cl :parachute))

(in-package :dijkstra)

;;; Now we can write the tests for the algorithm.
;;; All tests will be written in the test suite `dijkstra`.
(define-test dijkstra-test-suite)

;;; The first test is to check if the algorithm returns an empty list when the start vertex is the same as the end vertex.
(define-test test-same-vertex
      :parent dijkstra-test-suite
  (true (null (shortest-path (make-graph) :a :a))))

;;; Lets implement make-graph to create an empty graph.
(defun make-graph ()
  (make-hash-table))

;;; Lets implement the function shortest-path that receives a graph, a start vertex and an end vertex and returns the shortest path between the start and end vertices.
(defun shortest-path (graph start end)
  (if (eql start end)
      nil
      nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Run the test suite.
(parachute:test 'dijkstra-test-suite :report 'summary)
