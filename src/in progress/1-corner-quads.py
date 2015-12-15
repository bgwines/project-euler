# 2-corner diag cvx quads

import numpy
import fractions

# upper half z

n = 2
m = 2

nn = max(n,m)

diag_gcd_sums = []
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

print_diag_gcd_sums()

# round up (even if is int; int() rounds down)
diag_n = int(((-1.0 * n * (m - 1)) / m) + n) + 1

# NOT VERIFIED FOR (3,3)
# verified for (2, 2) and (3, 3) and (4, 4)!!! THIS IS INTERIOR Z ONLY
sum_oa = (1.0 / 48.0) * (n - 1) * (6 * m**2 * (n + 2) + 6 * m * (n - 2) - n * (3 * n**2 + 13 * n + 10)) - 0.5 * diag_gcd_sums[diag_n][m - 1]
sum_oc = sum_oa

# verified for (2, 2) and (3, 3) and (4, 4)!!! THIS IS INTERIOR Z ONLY
sum_d = (1.0 / 24.0) * (n - 1) * (6 * m**2 * n - 2 * m * (4 * n**2 + n + 12) + 3 * n**3 + n**2 + 10 * n + 24)

# verified for (2, 2) and (3, 3) and (4, 4)!!! THIS IS INTERIOR Z ONLY
sum_b_i = 0.25 * (n - 1) * (-4 + 2 * m**2 * (n - 1) + n**2 - m * (-6 + 3 * n + n**2) - (2 * m - n - 2) * fractions.gcd(n,m))

# verified for (2, 2) and (3, 3) and (4, 4)!!! THIS IS INTERIOR Z ONLY
sum_bo = 0.5 * (n - 1) * (4 + 2 * m**2 + m * (n - 6) - n**2)

# verified for (2, 2)
border_z = (n - 1) * (m - 1) * 2 * 3 * 2

##############################

(zn, zm) = (m, 1)

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

##############################

# (m, n) = (2, 2)
# (zm, zn) = (m, 1)

# (m, n) = (3, 3)
# (zm, zn) = (1, 0)
# 0.5 * (zn * zm + zn + zm - fractions.gcd(zn, zm))
# 0.5 * m * (m + 1) - 1
# 'a_c'
# (m - zm + 1) * (zn + 1) - (m - zm + 1) - zn - 1
# '-------------------------------------'

# (zm, zn) = (2, 0)
# 0.5 * (zn * zm + zn + zm - fractions.gcd(zn, zm))
# 0.5 * m * (m + 1) - 1
# 'a_c'
# (m - zm + 1) * (zn + 1) - (m - zm + 1) - zn - 1
# '-------------------------------------'

# (zm, zn) = (2, 1)
# 0.5 * (zn * zm + zn + zm - fractions.gcd(zn, zm))
# 0.5 * m * (m + 1) - 1
# 'a_c'
# (m - zm + 1) * (zn + 1) - (m - zm + 1) - zn - 1
# '-------------------------------------'

# (zm, zn) = (3, 1)
# 0.5 * (zn * zm + zn + zm - fractions.gcd(zn, zm))
# 0.5 * m * (m + 1) - 1
# 'a_c'
# (m - zm + 1) * (zn + 1) - (m - zm + 1) - zn - 1
# '-------------------------------------'

# (zm, zn) = (3, 2)
# 0.5 * (zn * zm + zn + zm - fractions.gcd(zn, zm))
# 0.5 * m * (m + 1) - 1
# 'a_c'
# (m - zm + 1) * (zn + 1) - (m - zm + 1) - zn - 1
# '-------------------------------------'

# (m, n) = (4, 4)
# (zm, zn) = (2, 1)

# (m, n) = (5, 5)
# (zm, zn) = (4, 2)
