#Determine the length of the shortest path from the given starting node s to every other node in the graph
#This uses a modified Bellman-Ford algorithm
def belman_ford(g, s):
	#Preallocate distance matrix
    distance = [0 for x in range(len(g.nodes))]
    for n in g.nodes():
        distance[n] = size
    distance[s] = 0
    
    #Petform the Bellman-Ford algorithm with weights set as 1
    for i in range(len(g)):
    	#Loop over each node
        for u in g.nodes():
        	#Loop over each neighbour of the current node
            for v in g[u]:
            	#Determine if a shorter path exists
                if distance[u] + 1 < distance[v]:
                    distance[v] = distance[u] + 1
    return distance

#Create a distance matrix of shortest paths.  Here the entries ij correspond to the length
#of the shortest path between nodes i and j, or 0 if no such path exists.
def get_shortest_paths(g):
    shortest_paths_matrix = []
    #Populate the distance matrix
    for n in g.nodes():
        paths = belman_ford(g,n)
        shortest_paths_matrix.append(paths)
    for i in range(len(shortest_paths_matrix)):
        for j in range(len(shortest_paths_matrix[0])):
        	#Set the distance to 0 if no path exists
            if shortest_paths_matrix[i][j] == size:
                shortest_paths_matrix[i][j] = 0
    return shortest_paths_matrix

shortest_paths_matrix = get_shortest_paths(g)
#Plot the shortest paths distance matrix
plt.figure(figsize=(16,14))
plt.subplot(222)
plt.title("Shortest Path Distance Matrix")
plt.imshow(shortest_paths_matrix,cmap = plt.cm.Blues)
plt.colorbar(orientation='vertical')

#This figure was included to compare the initial ring lattice with the rewired small-world network
plt.subplot(221)
plt.title("Initial Shortest Path Distance Matrix")
plt.imshow(initial_shortest_paths_matrix,cmap = plt.cm.Blues)
plt.colorbar(orientation='vertical')
plt.show()