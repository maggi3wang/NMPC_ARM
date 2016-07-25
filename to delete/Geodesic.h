/*
 * Geodesic.h
 *
 *  Created on: Jun 21, 2016
 *      Author: maggiewang
 */

#ifndef GEODESIC_H_
#define GEODESIC_H_

#include <math.h>
#include <vector>
#include <iostream>
#include <string>
#include <sstream>
#include <tuple>
#include <armadillo>
#include "setup_geodesic_calc_nofunc.h"
#include "libnpsol.h"

using namespace std;
using namespace arma;

struct GeodesicVars {
	mat T_e;
	mat T_dot_e;
};

struct Problem {
	// TODO
	string name;
	mat A;		// matrix with linear constraints, one constraint per row (dense or sparse)
	mat x_0;	// starting point
	mat x_L;	// lower bounds on variables x
	mat x_U;	// upper bounds on variables x
	mat b_L;
	mat b_U;
};

struct Result {
	// TODO

};

struct ComputedGeodesicTom {
	int ctrl_opt;	// not an int
	int e_rate;		// not an int
	int exit_flag;	// not an int (?)
};

//struct GeodesicReturnVars {
//	mat X;
//	mat X_dot;
//	mat J_opt;
//	string exit_flag;
//};

class Geodesic {

public:
	Geodesic();
	virtual ~Geodesic();
	void SetupGeodesicCalc(mat W_0, mat W_c);
	void ComputeGeodesic(mat start_p, mat end_p);

	// mat GetGeoWe();
	// // takes in problem, new linear constraints, lower bounds for linear constraints, 
	// // upper bounds for linear constraints

	// Problem ReplaceA(Problem prob, pair<mat, mat> Aeq, mat b_L, mat b_U);	
	
	// // accessors (maybe not necessary (?))
	// Problem GetProb();
	// GeodesicVars GetGeodesicVars();

private:
	int n, N;
	mxArray* geo_Prob;
	mxArray* K_e;
	mxArray* w_e;
	mxArray* T_e;
	mxArray* T_dot_e;
	mxArray* Aeq;
	void PrintGeodesicProblem();
	// pair<mat, mat> Clencurt(int K);
	// pair<mat, mat> ComputeCheby(int K, int t);	// is this necessary
	// pair<mat, mat> ComputeCheby(int K, mat t);
	// mat GeodesicCostTom(mat vars, int w, int K, int W_fnc);
	// mat GeodesicGrad(mat vars, int w, int K, mat T, int W_fnc, int dW_fnc);

	// void npsol();

	// mat start_p, end_p;
	// cube Phi, Phi_dot;
	// mat w_e;

	// // structs
	// Problem prob;
	// GeodesicVars geoVars;
	// Result res;

//	int t, w;
//	int T_e, T_dot_e;		// check that they are ints
//	vector<float> A_eq;
};

#endif /* GEODESIC_H_ */
