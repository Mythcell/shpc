#Implements the Barabasi-Albert generative model for preferential attachment.

size = 100 #Initial random graph size
probability = 0.05 #Edge probability for the random graph
n = 100 #Number of new nodes to add
m = 3 #Number of links for each new node to have

def pa_add_nodes(matrix, n, m):
    
    #Create a new matrix
    old_size = len(matrix)
    new_size = old_size + n
    new_matrix = np.zeros((new_size,new_size)).astype('int')
    
    #Merge the existing adjacency matrix into the new one
    for i in range(old_size):
        for j in range(old_size):
            new_matrix[i][j] = matrix[i][j]
    
    #Loop for each new node
    for a in range(old_size, new_size):
        #Determine the probabilities of each node
        total_degree = 0
        probabilities = []
        #First append the degree of each node...
        for node in g.nodes():
            probabilities.append(g.degree(node))
            total_degree += g.degree(node)
        #...then divide by the total degree to get probabilities
        probabilities[:] = [x / total_degree for x in probabilities]
        #Now choose m edges based on these probabilities
        new_edges = np.random.choice(len(g.nodes()), m, p=probabilities,replace=False)
        #Add the new node and the new edges to the graph
        g.add_node(a)
        for edge in new_edges:
            g.add_edge(a, edge)
            #Update the adjacency matrix
            new_matrix[a][edge] = 1
            new_matrix[edge][a] = 1
    return new_matrix

#Start off with a random graph
g = nx.Graph()
matrix = create_random_graph(size, probability)
edges = get_edges_from_matrix(matrix)
g.add_nodes_from(range(size))
g.add_edges_from(edges)
#print("Old matrix: ",matrix,"\n")

#Draw the random graph
plt.figure(figsize=(14,14))
plt.subplot(221)
nx.draw_circular(g, with_labels=True, node_color = colour)

matrix = pa_add_nodes(matrix, n, m)
#print("New matrix: ",new_matrix)

plt.subplot(222)
nx.draw_circular(g, with_labels=True, node_color = colour)
plt.show()