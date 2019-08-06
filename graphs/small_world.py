#Creates a Small World Network using the Watts-Strogatz Model.

size = 100 #Number of nodes
k = 2 #Number of neighbours on each side (this is K/2 in the literature)
p = 0.1 #Rewire probability

#This function creates the initial ring lattice-graph
def create_lattice_ring(size, k):
    #Initialise the adjacency matrix
    matrix = np.zeros((size,size)).astype('int')
    #Populate the adjacency matrix
    for i in range(size):
        count = 0
        for j in range(i, size):
            #Populate along the row / down the column k times
            for l in range(j, size):
                if (l+1 <= size-1 and count < k):
                    matrix[i][l+1] = 1
                    matrix[l+1][i] = 1
                    #Add wrap-around entries at the first and last node
                    if (l + size-k <= size-1):
                        matrix[i][size-k+l] = 1
                        matrix[size-k+l][i] = 1
                    count += 1
    return matrix

#Add an edge between two random nodes in the adjacency matrix
def add_random_edge(matrix):
    size = len(matrix)
    suitable_entry = False
    #Choose random i, j coordinates corresponding to an empty entry
    while suitable_entry == False:
        i, j = np.random.choice(size, 2, replace=False)
        if matrix[i][j] == 0:
            suitable_entry = True
    #Add the edge to the matrix
    matrix[i][j] = 1
    matrix[j][i] = 1

#Randomly rewires each edge with the given probability
def rewire_random(matrix, probability):
    size = len(matrix)
    for i in range(size):
        for j in range(i, size):
            #Check there exists an edge between i and j
            if matrix[i][j] != 0:
                if (np.random.rand() <= probability):
                    #Remove this edge
                    matrix[i][j] = 0
                    matrix[j][i] = 0
                    #Add a random edge
                    add_random_edge(matrix)
    return matrix

g = nx.Graph()
#Construct the initial graph from the adjacency matrix
matrix = create_lattice_ring(size, k)
edges = get_edges_from_matrix(matrix)
g.add_nodes_from(range(size))
g.add_edges_from(edges)
#print("Initial adjacency matrix:\n",matrix)

#Plot the initial graph
plt.figure(figsize=(14,14))
plt.subplot(221)
nx.draw_circular(g, with_labels=True, node_color = colour)
#Save shortest paths for the original ring lattice
initial_shortest_paths_matrix = get_shortest_paths(g)

#Now rewire the graph with probability p
matrix_new = rewire_random(matrix, p)
edges = get_edges_from_matrix(matrix_new)
#Construct the new, rewired graph
g = nx.Graph()
g.add_nodes_from(range(size))
g.add_edges_from(edges)
#print("New adjacency matrix:\n",matrix_new)

#Plot the rewired graph
plt.subplot(222)
nx.draw_circular(g, with_labels=True, node_color = colour)
plt.show()