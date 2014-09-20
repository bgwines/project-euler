//
//  main.cpp
//  Pathfinding
//
//  Created by Brett Wines on 12/22/13.
//  Copyright (c) 2013 Brett Wines. All rights reserved.
//

#include <iostream>
#include <vector>
#include <map>
#include <set>
#include "PQueue.h"
#include "types.h"
#include "grid.h"
using namespace std;

const int PROBLEM_NUM = 82;

void generateNodes(vector<Loc>& nodes, int numRows, int numCols) {
    for (int i=0; i<numRows; i++) {
        for (int j=0; j<numCols; j++) {
            nodes.push_back(makeLoc(i, j));
        }
    }
}

void initialize_node_colors(map<Loc, Color>& node_colors,
                            vector<Loc>& nodes,
                            Loc start) {
    for (int i=0; i<nodes.size(); i++) {
        node_colors.insert(make_pair(nodes[i], GRAY));
    }
    node_colors[start] = YELLOW;
}

void initialize_node_distances(map<Loc, double> node_distances,
                               vector<Loc>& nodes,
                               Loc start) {
    for (int i=0; i<nodes.size(); i++) {
        node_distances.insert(make_pair(nodes[i], 0));
    }
    node_distances[start] = numeric_limits<double>::max();
}

//sets all nodes to be their own parents
void initialize_node_parents(map<Loc, Loc>& node_parents,
                             vector<Loc>& nodes) {
    for (int i=0; i<nodes.size(); i++) {
        node_parents.insert(make_pair(nodes[i], nodes[i]));
    }
}

void update_neighbor(Loc curr_neighbor,
                     Loc curr_node,
                     Loc end,
                     map<Loc, double>& node_distances,
                     map<Loc, Color>& node_colors,
                     map<Loc, Loc>& node_parents,
                     PQueue<Loc>& nodes_pqueue,
                     double costFn(Loc from, Loc to, Grid<double>& world),
                     double heuristic(Loc from, Loc to, Grid<double>& world),
                     Grid<double>& world) {
    double L = costFn(curr_node, curr_neighbor, world);
    double d = node_distances[curr_node];
    
    if (node_colors[curr_neighbor] == GRAY) {
        node_colors[curr_neighbor] = YELLOW;
        node_parents[curr_neighbor] = curr_node;
        node_distances[curr_neighbor] = d + L;
        nodes_pqueue.enqueue(curr_neighbor,
                             (1) * (d + L + heuristic(curr_neighbor, end, world)));
    } else if (node_colors[curr_neighbor] == YELLOW
               && node_distances[curr_neighbor] > d + L) {
        node_parents[curr_neighbor] = curr_node;
        node_distances[curr_neighbor] = d + L;
        nodes_pqueue.decreaseKey(curr_neighbor,
                                 (1) * (d + L + heuristic(curr_neighbor, end, world)));
    }
}

vector<Loc> get_neighbors(Loc loc, Grid<double>& world) {
    vector<Loc> surrounding;
    if (PROBLEM_NUM == 82) {
        if (world.inBounds(loc.row + 1, loc.col)) {
            surrounding.push_back(makeLoc(loc.row + 1, loc.col));
        }
        if (world.inBounds(loc.row, loc.col + 1)) {
            surrounding.push_back(makeLoc(loc.row, loc.col + 1));
        }
    } else if (PROBLEM_NUM == 83) {
        for (int i=loc.row-1; i<=loc.row+1; i+=2) {
            if (world.inBounds(i, loc.col)) {
                surrounding.push_back(makeLoc(i, loc.col));
            }
        }
        int j = loc.col + 1;
        if (world.inBounds(loc.row, j)) {
            surrounding.push_back(makeLoc(loc.row, j));
        }
    } else if (PROBLEM_NUM == 84) {
        vector<Loc> surrounding;
        for (int i=loc.row-1; i<=loc.row+1; i+=2) {
            if (world.inBounds(i, loc.col)) {
                surrounding.push_back(makeLoc(i, loc.col));
            }
        }
        for (int j=loc.col-1; j<=loc.col+1; j+=2) {
            if (world.inBounds(loc.row, j)) {
                surrounding.push_back(makeLoc(loc.row, j));
            }
        }
    }
    return surrounding;
}

vector<Loc> make_path(Loc start,
                      Loc end,
                      map<Loc, Loc>& node_parents) {
    vector<Loc> path;
    Loc curr = end;
    while (curr != start) {
        path.push_back(curr);
        if (node_parents[curr] == curr) {
            cout << "UH-OH.\n";
            exit(0);
        }
        curr = node_parents[curr];
    }
    return path;
}

vector<Loc> findShortestPath(Loc start,
                             Loc end,
                             PQueue<Loc>& nodes_pqueue,
                             map<Loc, Color>& node_colors,
                             map<Loc, double>& node_distances,
                             map<Loc, Loc>& node_parents,
                             Grid<double>& world,
                             double costFn(Loc from, Loc to, Grid<double>& world),
                             double heuristic(Loc start, Loc end, Grid<double>& world)) {
    while (!nodes_pqueue.isEmpty()) {
        Loc curr_node = nodes_pqueue.dequeueMin();
        node_colors[curr_node] = GREEN;
        if (curr_node == end) {
            return make_path(start, end, node_parents);
        }
        
        vector<Loc> neighbors = get_neighbors(curr_node, world);
        for (int i=0; i<neighbors.size(); i++) {
            if (node_colors[neighbors[i]] == GREEN)
                continue;
            update_neighbor(neighbors[i],
                            curr_node,
                            end,
                            node_distances,
                            node_colors,
                            node_parents,
                            nodes_pqueue,
                            costFn,
                            heuristic,
                            world);
        }
    }
    //not reached
    return vector<Loc>();
}

vector<Loc>
shortestPath(Loc start,
             Loc end,
             Grid<double>& world,
             double costFn(Loc from, Loc to, Grid<double>& world),
             double heuristic(Loc start, Loc end, Grid<double>& world)) {
    vector<Loc> nodes;
    generateNodes(nodes, world.numRows(), world.numCols());
	
    map<Loc, Color> node_colors;
    initialize_node_colors(node_colors, nodes, start);
    
    map<Loc, double> node_distances;
    initialize_node_distances(node_distances, nodes, start);
    
    map<Loc, Loc> node_parents;
    initialize_node_parents(node_parents, nodes);
    
    PQueue<Loc> nodes_pqueue;
    nodes_pqueue.enqueue(start, heuristic(start, end, world));
    
    return findShortestPath(start,
                            end,
                            nodes_pqueue,
                            node_colors,
                            node_distances,
                            node_parents,
                            world,
                            costFn,
                            heuristic);
}

//Dijkstra
double h(Loc start, Loc end, Grid<double>& world) {
    return 0;
}

double c(Loc start, Loc end, Grid<double>& world) {
    return world[end.row][end.col];
}

const int DIM = 80;
//const double g_example[DIM][DIM] = {{131, 673, 234, 103, 18}, {201, 96, 342, 965, 150}, {630, 803, 746, 422, 111}, {537, 699, 497, 121, 956}, {805, 732, 524, 37, 331}};
const double g[DIM][DIM+2] = {{4445,2697,5115,718,2209,2212,654,4348,3079,6821,7668,3276,8874,4190,3785,2752,9473,7817,9137,496,7338,3434,7152,4355,4552,7917,7827,2460,2350,691,3514,5880,3145,7633,7199,3783,5066,7487,3285,1084,8985,760,872,8609,8051,1134,9536,5750,9716,9371,7619,5617,275,9721,2997,2698,1887,8825,6372,3014,2113,7122,7050,6775,5948,2758,1219,3539,348,7989,2735,9862,1263,8089,6401,9462,3168,2758,3748,5870}}//, {...

Grid<double> make_grid() {
    Grid<double> grid = Grid<double>(DIM, DIM);
    
    for (int i=0; i<DIM; i++) {
        for (int j=0; j<DIM; j++) {
            grid.set(i, j, g[i][j]);
        }
    }
    
    return grid;
}

int main(int argc, const char * argv[])
{
    Loc start = makeLoc(0, 0);
    Loc end = makeLoc(DIM-1, DIM-1);
    Grid<double> world = make_grid();
    
    vector<Loc> path = shortestPath(start, end, world, c, h);
    
    int sum = world[start.row][start.col];
    cout << "(" << start.row << "," << start.col << ") : " << world[start.row][start.col] << endl;
    for (int i=path.size()-1; i>=0; i--) {
        cout << "(" << path[i].row << "," << path[i].col << ") : " << world[path[i].row][path[i].col] << endl;
        sum += world[path[i].row][path[i].col];
    }
    
    cout << sum;
    
    return 0;
}

