
#include <assert.h>
#include <stdio.h>
#include <math.h>

const int N_ROWS = 1000;

int triangle[N_ROWS][N_ROWS];

int tri_sum_cache[N_ROWS][N_ROWS][N_ROWS] = {0};
int calc_tri_sum(int root_depth, int root_index, int depth) {
	if (depth <= 0)
		return 0;
	if (depth == 1)
		return triangle[root_depth][root_index];

	if (tri_sum_cache[root_depth][root_index][depth] != 0) {
		return tri_sum_cache[root_depth][root_index][depth];
	}

	int tri_sum = triangle[root_depth][root_index];
	tri_sum += calc_tri_sum(root_depth+1, root_index+1, depth-1);
	tri_sum += calc_tri_sum(root_depth+1, root_index  , depth-1);
	tri_sum -= calc_tri_sum(root_depth+2, root_index+1, depth-2);

	tri_sum_cache[root_depth][root_index][depth] = tri_sum;
	return tri_sum;
}

long seq[500500];
void linear_congrential_generator() {
	long t = 0;
	for (int k=1; k<=500500; k++) {
		t = ((615949 * t) + 797807) % (int)pow(2, 20);
		seq[k-1] = t - pow(2, 19);
	}
}

void init_triangle() {
	linear_congrential_generator();

	int seq_index = 0;
	for (int row=0; row<N_ROWS; row++) {
		int row_length = row + 1;
		for (int col=0; col<row_length; col++) {
			triangle[row][col] = seq[seq_index];
			seq_index++;
		}
	}
}

int main(int argc, char const *argv[])
{
	init_triangle();

	int min = 1000000;
	for (int root_depth=0; root_depth<N_ROWS; root_depth++) {
		int row_length = root_depth + 1;
		for (int root_index=0; root_index<row_length; root_index++) {
			for (int depth=1; depth<(N_ROWS - root_depth + 1); depth++){
				int tri_sum = calc_tri_sum(root_depth, root_index, depth);
				if (tri_sum < min) {
					min = tri_sum;
				}
			}
		}
	}
	printf("%d\n", min);

	return 0;
}
