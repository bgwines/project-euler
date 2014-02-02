
import pdb

MIN_LEN = 3
DESIRED_LEN = 7

def print_tiling(a_len, placement_len, b_len):
    print '\t|',
    for i in xrange(a_len+1):
        print '  |',
    for i in xrange(placement_len):
        print '* |',
    for i in xrange(b_len+1):
        print '  |',
    print ''

def calc_num_tilings(n_tiles_before_placement, placement_len, arr_len, dp_arr):
    a_len = max(0, -1 + n_tiles_before_placement)
    b_len = max(0, -1 + arr_len - n_tiles_before_placement - placement_len)
    #2* below because you can either make this placement, or not.

    print '\tf(#before: ', n_tiles_before_placement,
    print 'placement_len: ', placement_len,
    print 'arr_len: ', arr_len,
    if dp_arr[a_len] + dp_arr[b_len] == 0:
        print 'score: ', 1,
        print_tiling(-1 + n_tiles_before_placement, placement_len, -1 + arr_len - n_tiles_before_placement - placement_len)
        return 1
    else:
        print 'score: ', 2 * (dp_arr[a_len] + dp_arr[b_len]),
        print_tiling(-1 + n_tiles_before_placement, placement_len, -1 + arr_len - n_tiles_before_placement - placement_len)
        return 2 * (dp_arr[a_len] + dp_arr[b_len])

def calc_num_tilings_in_arr_of_len(arr_len, dp_arr):
    #pdb.set_trace()
    placement_lengths = []
    l = MIN_LEN
    while l <= arr_len:
        placement_lengths.append(l)
        l += 1
    sum = 0
    for placement_length in placement_lengths:
        for placement_location in xrange(arr_len - placement_length + 1):
            sum += calc_num_tilings(
                placement_location, #0-indexed => same thing
                placement_length,
                arr_len,
                dp_arr
            )
    return sum

dp_map = {}
for i in xrange(DESIRED_LEN+1): #len is not 0-indexed
    if i == 0:
        dp_map[i] = 0
        continue
    #pdb.set_trace()
    dp_map[i] = calc_num_tilings_in_arr_of_len(i, dp_map)
    print 'len: ', i, ', min_len: ', MIN_LEN, ', num_ways: ', dp_map[i]
    print ''

#doesn't account for not placing remaining ones?
print dp_map[DESIRED_LEN]