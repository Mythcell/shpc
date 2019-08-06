#Helper methods to convert between adjacency matrices and adjacency lists

def matrix_to_adjlist(matrix):
    """Converts an adjacency matrix into an adjacency list"""
    size = len(matrix)
    assert size == len(matrix[0])
    #Here adj_list[i] will contain a list of neighbours to i
    adjlist = []
    for i in range(size):
        i_list = []
        for j in range(size):
            if matrix[i][j] != 0:
                i_list.append(j)
        adjlist.append(i_list)
    return adjlist

def adjlist_to_matrix(adjlist):
    """Converts an adjacency list into an adjacency matrix"""
    size = len(adjlist)
    matrix = np.zeros((size,size),dtype='int')
    for i in range(size):
        for j in adjlist[i]:
            matrix[i][j] = 1
    return matrix

def edges_from_matrix(matrix):
    """Returns a list of all edge pairs from an adjacency matrix"""
    edges = []
    for i in range(len(matrix)):
        for j in range(i,len(matrix)):
            if matrix[i][j] != 0:
                edge = [i,j]
                edges.append(edge)
    return edges

def edges_from_adjlist(adjlist):
    """Returns a list of all edge pairs from an adjacency list"""
    edges = []
    for i in range(len(adjlist)):
        #Skip nodes without edges
        if len(adjlist[i]) == 0:
            continue
        for j in adjlist[i]:
            edge = sorted([i,j])
            #Avoid duplicates
            if edge not in edges:
                edges.append(edge)
    return edges