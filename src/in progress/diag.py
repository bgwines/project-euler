
import pdb

n = 6
m = 9

nn = 5

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


print_gcd_table()
print_diag_gcd_sums()