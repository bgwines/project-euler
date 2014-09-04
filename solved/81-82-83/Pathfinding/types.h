/******************************************************************************
 * File: TrailblazerTypes.h
 *
 * Fundamental types for the Trailblazer assignment.
 */
 
enum Color {
    GRAY, YELLOW, GREEN
};

struct Loc {
    int row;
    int col;
};

Loc makeLoc(int row, int col);

struct Edge {
		Loc start;
		Loc end;
};

Edge makeEdge(Loc start, Loc end);


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* The functions below this point are provided for convenience and are not
 * strictly necessary for your implementation.
 */
 
/* Comparison operators for Loc and Edge.	 Our Map and Set type cannot store
 * custom structs as keys (for a map) or values (for a set) unless they can be
 * compared with the relational operators.	You will probably not directly use
 * these in your solution, but you're welcome to do so if you find them useful.
 */
bool operator <	 (Loc lhs, Loc rhs);
bool operator >	 (Loc lhs, Loc rhs);
bool operator == (Loc lhs, Loc rhs);
bool operator != (Loc lhs, Loc rhs);
bool operator <= (Loc lhs, Loc rhs);
bool operator >= (Loc lhs, Loc rhs);

bool operator <	 (Edge lhs, Edge rhs);
bool operator >	 (Edge lhs, Edge rhs);
bool operator == (Edge lhs, Edge rhs);
bool operator != (Edge lhs, Edge rhs);
bool operator <= (Edge lhs, Edge rhs);
bool operator >= (Edge lhs, Edge rhs);
