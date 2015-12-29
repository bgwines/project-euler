
import fractions
import numpy
import pdb

fact_memo = {}
def fact(n):
    if n == 0:
        return 1

    if n not in fact_memo:
        fact_memo[n] = n * fact(n - 1)

    return fact_memo[n]

def choose(n, k):
    return fact(n) / (fact(k) * fact(n-k))

'''
\sum_lb^ub gcd(i,k)
'''
def sum_gcd((lb, ub), k):
    return gcd_sums[k][ub] - gcd_sums[k][lb-1]

def num_0_corner_quads_all_edges(n, m):
    # = (n + 1 - 2) # l vertex choices
    # * (n + 1 - 2) # r vertex choices
    # * (m + 1 - 2) # u vertex choices
    # * (m + 1 - 2) # d vertex choices
    # `+ 1`s for 0-indexing of `n` and `m`; need to count # of points on edge
    # `- 2`s for no vertices in corners
    return pow((n + 1 - 2), 2) * pow((m + 1 - 2), 2)

def num_1_corner_quads(n, m):
    num_1_corner_triangle_boundary_points\
        = (n - 1) * sum_gcd((1,n-1),m)\
        + (m - 1) * sum_gcd((1,m-1),n)\
        + double_gcd_sums[n-1][m-1]\
        - double_gcd_sums[n-1][0]\
        - double_gcd_sums[0][m-1]
    num_1_corner_quads_cve = 4 * 3 *\
        ( area_sums[n - 1][m - 1]\
        + (n - 1) * (m - 1)\
        - 0.5 * num_1_corner_triangle_boundary_points )

    num_1_corner_quads_cvx = 2 *\
        ( 0.25 * (m - 1) * (n - 1) * (m * (n + 2) + n - 4) - 0.5 * sum_gcd((1, m - 1), n)\
        + 0.25 * (n - 1) * (m - 1) * (n * (m + 2) + m - 4) - 0.5 * sum_gcd((1, n - 1), m)\
        + 0.125 * (m - 1) * (n - 1) * (m * (n + 2) + 2 * (n - 4)) - 0.5 * (double_gcd_sums[n-1][m-1] - double_gcd_sums[n-1][0] - double_gcd_sums[0][m-1]) )
    num_1_corner_quads_cvx += 2 *\
        ( 0.25 * (m - 1) * (n - 1) * (m * (n + 2) + n - 4) - 0.5 * sum_gcd((1, m - 1), n)\
        + 0.25 * (n - 1) * (m - 1) * (n * (m + 2) + m - 4) - 0.5 * sum_gcd((1, n - 1), m)\
        + 0.125 * (n - 1) * (m - 1) * (n * (m + 2) + 2 * (m - 4)) - 0.5 * (double_gcd_sums[n-1][m-1] - double_gcd_sums[n-1][0] - double_gcd_sums[0][m-1]) )

    return num_1_corner_quads_cve + num_1_corner_quads_cvx

'''
x .__________________
  |\_----   a  | d  |
  |  \__ \-----.z___|
  |     \__     \ c |
  |        \__   \  |
 b_o         \__  \ |
  |       b_i   \__\|
  |                \|
  |_____b_o_________.y

unsummed:

# verified!!!1!!1!11!!!!!
oa = 0.5 * (zn * zm + zn + zm - fractions.gcd(zn, zm))

# verified!!!1!!1!11!!!!!
b_i = 0.5 * (m * n - n - m - fractions.gcd(n,m)) + 1

# verified!!!1!!1!11!!!!!
b_o = n + m - 2

# verified!!!1!!1!11!!!!!
oc = 0.5 * (zn * zm + zn + zm - fractions.gcd(zn, zm))

# verified!!!1!!1!11!!!!!
d = (m - zm + 1) * (zn + 1) - (m - zm + 1) - zn - 1

'''
def num_2_corner_quads_diag_cvx(n, m):
    # `2 *`s at start for lower half of zs

    # round up (even if is int; int() rounds down)
    # n - [...] for opposite direction of n-indexing compared to cartesian plane
    diag_n = n - (int(((-1.0 * n * (m - 1)) / m) + n) + 1)

    # verified for (2, 2) and (3, 3) and (4, 4)!!! THIS IS INTERIOR Z ONLY
    sum_oa = 2 * (1.0 / 48.0) * (n - 1) * (6 * m**2 * (n + 2) + 6 * m * (n - 2) - n * (3 * n**2 + 13 * n + 10)) - 0.5 * diag_gcd_sums[diag_n][m - 1]

    sum_oc = sum_oa # symmetry

    # verified for (2, 2) and (3, 3) and (4, 4)!!! THIS IS INTERIOR Z ONLY
    sum_d = 2 * (1.0 / 24.0) * (n - 1) * (6 * m**2 * n - 2 * m * (4 * n**2 + n + 12) + 3 * n**3 + n**2 + 10 * n + 24)

    # NO `* 2` HERE because otherwise double-counting
    # verified for (2, 2) and (3, 3) and (4, 4)!!! THIS IS INTERIOR Z ONLY
    sum_b_i = 0.25 * (n - 1) * (-4 + 2 * m**2 * (n - 1) + n**2 - m * (-6 + 3 * n + n**2) - (2 * m - n - 2) * fractions.gcd(n,m))

    # verified for (2, 2) and (3, 3) and (4, 4)!!! THIS IS INTERIOR Z ONLY
    sum_b_o = 2 * 0.5 * (n - 1) * (4 + 2 * m**2 + m * (n - 6) - n**2)

    # verified for (2, 2)
    # `* 3` for 3 sides for w
    # `* 2` for other z locations
    # `* 2` for other diagonal
    border_z_quads = (n - 1) * (m - 1) * 2 * 3 * 2

    return sum_oa + sum_oc + sum_d + sum_b_i + sum_b_o + border_z_quads

def num_2_corner_quads_diag_cve(n, m):
    # inner z

    # below coment is VERIFIED
    # (n + 1) * (m + 1) - (oa + b_i + b_o + oc + d + 3 + gcd_table[n][m] + 1)

    # (* 2) for other half of inner z
    # stop double-guessing yourself here; this is right
    all_inner_z_pts_sum = 2 * 0.5 * (m + 1) * (n**2 - 1) * (2 * m - n - 2)

    # round up (even if is int; int() rounds down)
    # n - [...] for opposite direction of n-indexing compared to cartesian plane
    diag_n = n - (int(((-1.0 * n * (m - 1)) / m) + n) + 1)

    # verified for (2, 2) and (3, 3) and (4, 4)!!! THIS IS INTERIOR Z ONLY
    sum_oa = 2 * (1.0 / 48.0) * (n - 1) * (6 * m**2 * (n + 2) + 6 * m * (n - 2) - n * (3 * n**2 + 13 * n + 10)) - 0.5 * diag_gcd_sums[diag_n][m - 1]

    sum_oc = sum_oa # symmetry

    # verified for (2, 2) and (3, 3) and (4, 4)!!! THIS IS INTERIOR Z ONLY
    sum_d = 2 * (1.0 / 24.0) * (n - 1) * (6 * m**2 * n - 2 * m * (4 * n**2 + n + 12) + 3 * n**3 + n**2 + 10 * n + 24)

    # NO `* 2` HERE because otherwise double-counting
    # verified for (2, 2) and (3, 3) and (4, 4)!!! THIS IS INTERIOR Z ONLY
    sum_b_i = 0.25 * (n - 1) * (-4 + 2 * m**2 * (n - 1) + n**2 - m * (-6 + 3 * n + n**2) - (2 * m - n - 2) * fractions.gcd(n,m))

    # verified for (2, 2) and (3, 3) and (4, 4)!!! THIS IS INTERIOR Z ONLY
    sum_b_o = 2 * 0.5 * (n - 1) * (4 + 2 * m**2 + m * (n - 6) - n**2)

    inner_z_interior_ws = all_inner_z_pts_sum - sum_oa - sum_oc - sum_d - sum_b_i - sum_b_o

    # border z

    outer_z_interior_ws

    return inner_z_interior_ws + outer_z_interior_ws


def num_2_corner_quads_diag(n, m):
    return num_2_corner_quads_diag_cvx(n, m)\
        +  num_2_corner_quads_diag_cve(n, m)

def num_2_corner_quads(n, m):
    # *all* the 2-corner nodiag cve quads; this is confirmed correct
    # cve
    num_2_corner_quads_nodiag = 3 *\
    2 *\
        ( ( 0.5 * n * pow((m - 1), 2) - sum_gcd((1, m - 1), n) + (m - 1))\
        + ( 0.5 * m * pow((n - 1), 2) - sum_gcd((1, n - 1), m) + (n - 1)) )
    # cvx
    num_2_corner_quads_nodiag += 2 *\
        ( (m - 1) * (n * m * 0.5 + n + 0.5 * m - 2) - sum_gcd((1, m - 1), n)
        + (n - 1) * (m * n * 0.5 + m + 0.5 * n - 2) - sum_gcd((1, n - 1), m) )

    return num_2_corner_quads_nodiag + num_2_corner_quads_diag(n, m)

def num_3_corner_quads(n, m):
    # * 4 for symmetry; no risk of double-counting in this scenario
    # * 3 for concavity
    # rest of formula is by Pick's theorem
    num_3_corner_quads_cve = 4 * 3 * ((m * n + 2 - n - m - gcd_table[n][m]) / 2)
    # (m - 1) and (n - 1) are the boundary ws
    num_3_corner_quads_cvx = 4 * (num_3_corner_quads_cve / 3 + (m - 1) + (n - 1))
    return num_3_corner_quads_cve + num_3_corner_quads_cvx

'''
 (1)
 = f(n, m - 1) * 2      # intersecting recursive segments
 - f(n, m - 2)          # don't double-count intersection

 (2)
 + f(n - 1, m)          # get top 9/10 quads to cover using L and R edges
 - 2 + f(n - 1, m - 1)  # don't double-count from section (1)
 + f(n - 1, m - 2)      # don't remove this part twice in above line

 (3)
 + f(n - 1, m)          # get bottom 9/10 quads to cover using L and R edges
 - 2 + f(n - 1, m - 1)  # don't double-count from section (1)
 + f(n - 1, m - 2)      # don't remove this part twice in above line
'''
def f_recursive_case(n, m):
    return f(n, m - 1) * 2\
    - f(n, m - 2)\
\
    + f(n - 1, m)\
    - 2 * f(n - 1, m - 1)\
    + f(n - 1, m - 2)\
\
    + f(n - 1, m)\
    - 2 * f(n - 1, m - 1)\
    + f(n - 1, m - 2)

f_memo = {}
def f(n, m):
    if n < 0 or m < 0:
        raise Exception('Fatal: invalid parameters: (' + str(n) + ', ' + str(m) + ')')

    if n > m:
        return f(m, n) # WLOG

    if n == 0 or m == 0:
        return 0

    if (n, m) not in f_memo:
        if n == 1:
            return pow(choose(m + 1, 2), 2) # `+ 1` because 0-indexing of `n` and `m`

        print '-------------------------------'
        print n,m
        print ''
        print '# recursive  quads           = ', f_recursive_case(n, m)
        print 'num_0_corner_quads_all_edges = ', num_0_corner_quads_all_edges(n, m)
        print 'num_1_corner_quads           = ', num_1_corner_quads(n,m)
        print 'num_2_corner_quads           = ', num_2_corner_quads(n,m)
        print 'num_3_corner_quads           = ', num_3_corner_quads(n, m)
        print 'num_4_corner_quads           = ', 1

        f_memo[(n, m)]\
            = f_recursive_case(n, m)\
            + num_0_corner_quads_all_edges(n, m)\
            + num_1_corner_quads(n,m)\
            + num_2_corner_quads(n,m)\
            + num_3_corner_quads(n, m)\
            + 1 # 4 corners
    return f_memo[(n, m)]

'''
gcd_sums[a][b] is \sum_{i=0}^a gcd(i,b)
'''
gcd_table = []
gcd_sums = []
double_gcd_sums = []
area_sums = []
diag_gcd_sums = []
def init_lookup_tables(n, m):
    nn = max(n, m)
    # gcd_table
    for a in xrange(0, nn + 1):
        gcd_table.append([])
        for b in xrange(0, nn + 1):
            gcd = None
            if a == 0:
                gcd = b
            elif b == 0:
                gcd = a
            elif a == b:
                gcd = a
            else:
                if a > b:
                    aa = a - b
                    bb = b
                else:
                    aa = a
                    bb = b - a

                if aa == 0:
                    gcd = bb
                elif bb == 0:
                    gcd = aa
                elif aa == bb:
                    gcd = aa

                if aa > bb:
                   gcd = gcd_table[aa - bb][bb]
                else:
                   gcd = gcd_table[aa][bb - aa]

            gcd_table[a].append(gcd)

    # gcd_sums
    for i in xrange(0, nn + 1):
        gcd_sums.append([]) # j = 0
        s = 0
        for j in xrange(0, nn + 1):
            s += gcd_table[i][j]
            gcd_sums[i].append(s)

    # double_gcd_sums
    for i in xrange(0, nn + 1):
        double_gcd_sums.append([]) # j = 0
        for j in xrange(0, nn + 1):
            recL = double_gcd_sums[i][j-1] if j != 0 else 0
            recR = double_gcd_sums[i-1][j] if i != 0 else 0
            intersection = double_gcd_sums[i-1][j-1] if i != 0 and j != 0 else 0
            double_gcd_sum = recL + recR - intersection + fractions.gcd(i,j)
            double_gcd_sums[i].append(double_gcd_sum)

    # area_sums
    area_sums.append([0 for _ in xrange(nn + 1)]) # inner [] for y = 0 case
    for y in range(1, nn + 1):
        area_sums.append([0]) # 0 for z = 0 case
        for z in range(1, nn + 1):
            recL = area_sums[y][z-1] if z > 1 else 0
            recR = area_sums[y-1][z] if y > 1 else 0
            intersection = area_sums[y-1][z-1] if y > 1 and z > 1 else 0
            a = pow(m,2) + pow(y,2)
            b = pow(n - y, 2) + pow(m - z, 2)
            c = pow(n, 2) + pow(z, 2)
            area = 0.25 * numpy.sqrt(4 * a * b - pow((a + b - c), 2))
            area_sums[y].append(recL + recR - intersection + area)

    # diag_gcd_sums
    for i in xrange(0, n + 1):
        diag_gcd_sums.append([]) # j = 0
        for j in xrange(0, m + 1):
            # if (j, i) == (5, 3):
            #     pdb.set_trace()
            if i == 0 or j == 0 or j == m or ((n - i) <= ((-1.0 * n * j) / m) + n):
                diag_gcd_sums[i].append(0)
            else:
                recJ = diag_gcd_sums[i][j-1] if j != 0 else 0
                recI = diag_gcd_sums[i-1][j] if i != 0 else 0
                intersection = diag_gcd_sums[i-1][j-1] if i != 0 and j != 0 else 0
                if recJ == 0:
                    print 'HERE: ', (j, i)
                    recJ = intersection
                diag_gcd_sum = recJ + recI - intersection + fractions.gcd(i, j)
                diag_gcd_sums[i].append(diag_gcd_sum)

def print_gcd_table():
    print 'gcd table:'
    print '      ',
    for i in xrange(0, len(gcd_table)):
        print i,
        print '  ',
    print ''
    print '   ',
    print '-' * (len(gcd_table) * 5)
    for i in xrange(0, len(gcd_table)):
        print '  ' + str(i) + ' | ',
        for j in xrange(0, len(gcd_table[i])):
            print gcd_table[i][j],
            print '  ',
        print ''

def print_double_gcd_sums():
    print 'double gcd sums:'
    print '      ',
    for i in xrange(0, len(double_gcd_sums)):
        print i,
        print '  ',
    print ''
    print '   ',
    print '-' * (len(double_gcd_sums) * 5)
    for i in xrange(0, len(double_gcd_sums)):
        print '  ' + str(i) + ' | ',
        for j in xrange(0, len(double_gcd_sums[i])):
            print double_gcd_sums[i][j],
            print '  ',
        print ''

def print_area_sums():
    print 'area sums:'
    print '      ',
    for i in xrange(0, len(area_sums)):
        print i,
        print '  ',
    print ''
    print '   ',
    print '-' * (len(area_sums) * 5)
    for i in xrange(0, len(area_sums)):
        print '  ' + str(i) + ' | ',
        for j in xrange(0, len(area_sums[i])):
            print area_sums[i][j],
            print '  ',
        print ''

# print sum_gcd((3,9),6) # should be 18
def print_gcd_sums_table():
    print ''
    print 'gcd sums:'
    print '      ',
    for i in xrange(0, len(gcd_sums)):
        print i,
        print '  ',
    print ''
    print '   ',
    print '-' * (len(gcd_sums) * 5)
    for i in xrange(0, len(gcd_sums)):
        print '  ' + str(i) + ' | ',
        for j in xrange(0, len(gcd_sums[i])):
            ss = gcd_sums[i][j]
            if gcd_sums[i][j] < 10:
                ss = str(gcd_sums[i][j]) + ' '
            print ss,
            print ' ',
        print ''

def print_diag_gcd_sums():
    print 'diag gcd sums:'
    print '      ',
    for i in xrange(0, len(diag_gcd_sums[0])):
        print i,
        print '  ',
    print ''
    print '   ',
    print '-' * (len(diag_gcd_sums[0]) * 5)
    for i in xrange(0, len(diag_gcd_sums)):
        print '  ' + str(i) + ' | ',
        for j in xrange(0, len(diag_gcd_sums[i])):
            print diag_gcd_sums[i][j],
            print '  ',
        print ''

def main():
    n = 2
    m = 2
    print n, m

    init_lookup_tables(n, m)
    print_gcd_table()
    print_double_gcd_sums()
    print_gcd_sums_table()
    print_area_sums()
    print_diag_gcd_sums()

    print f(n, m)

if __name__ == '__main__':
    main()
