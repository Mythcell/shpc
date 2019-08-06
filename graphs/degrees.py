#Helper functions to manually determine the degrees of nodes in a graph

def determine_degrees(matrix):
    adjlist = matrix_to_adjlist(matrix)
    return [len(i) for i in adjlist]

#Of course this can also be done by strictly using an adjacency matrix
def determine_degrees_matrix(matrix):
    size = len(matrix)
    degs = [0 for i in range(size)]
    for i in range(size):
        d = 0
        for j in range(size):
            d += 1 if matrix[i][j] == 1 else 0
        degs[i] = d
    return degs