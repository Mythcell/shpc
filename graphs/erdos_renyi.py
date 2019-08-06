#Create an Erdos-Renyi graph using the (n,p) and (n,M) methods
#i.e randomly assign edges between n nodes with a probability p
#or randomly assign M edges between n nodes

import networkx as nx
import numpy as np
import matplotlib.pyplot as plt

size = 100 #number of nodes
probability = 0.1 #probability that two nodes are connected

def create_random_graph_p(size,probability):
    """Creates a random Erdos-Renyi graph using the (n,p) model.
    Returns an adjacency matrix"""

    matrix = np.zeros((size,size),dtype='int')
    p = max(probability,0)
    
    #The matrix is populated by randomlyadding edges between nodes
    #Avoids self loops (i.e i != j) and duplicates (j >= i)
    for i in range(size):
        #Only consider upper right triangle as matrix is symmetric
        for j in range(i,size):
            if np.random.rand() <= p and i != j:
                matrix[i][j] = 1
                matrix[j][i] = 1
    return matrix

def create_random_graph_m(size,n_edges):
    """Creates a random Erdos-Renyi graph using the (n,M) model.
    Returns an adjacency matrix"""
    
    matrix = np.zeros((size,size),dtype='int')
    #Restrict m to the maximum number of possible edges
    m = min(int((size*(size-1))/2),n_edges)
    m = max(m,0)
    
    #The matrix is populated by choosing two random nodes (n_edges) times
    #There must be n_edge unique node pairs
    chosen = []
    n_added = 0
    while n_added < m:
        node_pair = sorted(np.random.choice(size, 2, replace=False).tolist())
        #Only add the edge if the node_pair hasn't already been picked
        if node_pair not in chosen:
            matrix[node_pair[0],node_pair[1]] = 1
            matrix[node_pair[1],node_pair[0]] = 1
            chosen.append(node_pair)
            n_added += 1
    return matrix

#This function returns an array of node pairs from an adjacency matrix
def get_edges_from_matrix(matrix):
    
    size = len(matrix)
    #Check that the matrix is a square matrix
    assert (len(matrix) == size and len(matrix[0] == size))
    
    edges = [] #Edges to be repesented by node pairs
    
    for i in range(size):
        #Ignore the lower triangle to avoid duplicating entries
        for j in range(i, size):
            if matrix[i][j] == 1:
                edge = [i, j] #Obtain the node pair
                edges.append(edge)
    return edges
    
#First initialise an empty graph
g = nx.Graph()
#Now get the adjacency matrix
matrix = create_random_graph_p(size, probability)
#Parse the edges from the adjacency
edges = get_edges_from_matrix(matrix)
#Construct the graph
g.add_nodes_from(range(size))
g.add_edges_from(edges)

#Output adjacency matrix and list of edges (node pairs)
#print(matrix,"\n")
#print(edges,"\n")
print("Number of edges: ",len(edges),"( Max:",(size*(size-1)/2),")")

#Draw the graphs
plt.figure(figsize=(12,12))
colour = [0.7, 0.7, 1, 1]
plt.subplot(221)
nx.draw(g, with_labels=True, node_color = colour)
plt.subplot(222)
nx.draw_circular(g, with_labels=True, node_color = colour)
plt.show()