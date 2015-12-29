
import pdb

def take(n, l):
    l2 = []
    for (i, e) in enumerate(l):
        if (i == n):
            break;
        l2.append(e)
    return l2

def drop(n, l):
    l2 = []
    for (i, e) in enumerate(l):
        if (i >= n):
            l2.append(e)
    return l2

# inclusive in all arguments
def best_matrix_sum(matrix, min_x):
    best_sum = 0
    for y in xrange(0, max_y + 1): # non-inclusive in 2nd arg
        u_matrix =
        curr_sum\
            = matrix[y][min_x]\
            + best_matrix_sum(u_matrix, min_x + 1)\
            + best_matrix_sum(d_matrix, min_x + 1)

def main():
    matrix =\
        [ [  7,  53, 183, 439, 863]
        , [497, 383, 563,  79, 973]
        , [287,  63, 343, 169, 583]
        , [627, 343, 773, 959, 943]
        , [767, 473, 103, 669, 303] ]

    print best_matrix_sum(matrix, 0)

if __name__ == '__main__':
    main()