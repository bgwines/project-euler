/******************************************************************************
 * File: TrailblazerTypes.cpp
 *
 * Implementation of the helper functions from TrailblazerTypes.h.
 */

#include "types.h"

Loc makeLoc(int row, int col) {
	Loc result = { row, col };
	return result;
}
Edge makeEdge(Loc start, Loc end) {
	Edge result = { start, end };
	return result;
}

bool operator < (Loc lhs, Loc rhs) {
		if (lhs.row != rhs.row)
				return lhs.row < rhs.row;
		return lhs.col < rhs.col;
}

bool operator > (Loc lhs, Loc rhs) {
	return rhs < lhs;
}

bool operator <= (Loc lhs, Loc rhs) {
	return !(rhs < lhs);
}

bool operator >= (Loc lhs, Loc rhs) {
	return !(lhs < rhs);
}

bool operator == (Loc lhs, Loc rhs) {
	return lhs.row == rhs.row && lhs.col == rhs.col;
}

bool operator != (Loc lhs, Loc rhs) {
	return !(lhs == rhs);
}

bool operator < (Edge lhs, Edge rhs) {
	if (lhs.start != rhs.start) return lhs.start < rhs.start;
	return lhs.end < rhs.end;	
}

bool operator > (Edge lhs, Edge rhs) {
	return rhs < lhs;
}

bool operator <= (Edge lhs, Edge rhs) {
	return !(rhs < lhs);
}

bool operator >= (Edge lhs, Edge rhs) {
	return !(lhs < rhs);
}

bool operator == (Edge lhs, Edge rhs) {
	return lhs.start == rhs.start && lhs.end == rhs.end;
}

bool operator != (Edge lhs, Edge rhs) {
	return !(lhs == rhs);
}
