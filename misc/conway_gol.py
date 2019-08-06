import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

#These are our initial parameters
w = 384
h = 256
p = 0.1
nframes = 100

def initial_state(p):
    """Creates an initial state"""
    #Initialise an empty state (all set to 0)
    state = [[0 for x in range(w)] for x in range(h)]

    #Now randomly populate the state space
    for i in range(len(state)):
        for j in range(len(state[i])):
            state[i][j] = 1 if np.random.rand() < p else 0
    return state

def check_neighbours(i, j):
    """Checks the neighbours of a given cell to determine
    whether the cell lives or dies in the next iteration"""
    #First count the number of live neighbours
    w = len(state); h = len(state[0])
    ip = (i+1)%(w); im = (w+i-1)%(w); jp = (j+1)%(h); jm = (h+j-1)%(h)
    ln = (state[ip][jp] + state[ip][j] + state[ip][jm] + state[i][jp] + state[i][jm] + 
    state[im][jp] + state[im][j] + state[im][jm])
    #Logic for the cellular automaton
    if ln < 2 or ln > 3:
        return 0
    elif (ln == 2 or ln == 3) and state[i][j] == 1:
        return 1
    elif ln == 3 and state[i][j] == 0:
        return 1
    else:
        return 0

def get_next_state():
    new_state = [[0 for x in range(w)] for x in range(h)]
    for i in range(len(new_state)):
        for j in range(len(new_state[i])):
            new_state[i][j] = check_neighbours(i, j)
    return new_state

def evolve(i):
    global state
    mat.set_data(state)
    new_state = get_next_state()
    state = new_state
    return [mat]

state = initial_state(p)

fig = plt.figure(figsize=(12,8))
axes = fig.add_subplot(1,1,1)
mat = plt.imshow(state,interpolation="none")
mat.set_cmap('binary')
plt.xticks([]); plt.yticks([])
fig.tight_layout()
ani = animation.FuncAnimation(fig, evolve, frames=nframes)
ani.save('test.mp4',fps=10)