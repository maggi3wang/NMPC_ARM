/*
 * AuxiliaryController.cpp
 *
 *  Created on: July 18, 2016
 *      Author: maggiewang
 */

#include "AuxiliaryController.h"
// #include "armaMex.hpp"
#include "f2c.h"
#include <chrono>
#include <limits>

using namespace std::chrono;

extern "C" int qpopt(integer);

AuxiliaryController::AuxiliaryController() {
	// n = 4;	// change
	// N = 4; // change

	// // check these numbers
	// geo_Prob = (mxArray*)mxMalloc(100000 * sizeof(double));
	// K_e = (mxArray*)mxMalloc(100 * sizeof(double));
	// w_e = (mxArray*)mxMalloc(100 * sizeof(double));
	// T_e = (mxArray*)mxMalloc(100 * sizeof(double));
	// T_dot_e = (mxArray*)mxMalloc(100 * sizeof(double));
	// Aeq = (mxArray*)mxMalloc(100 * sizeof(double));
	// mxFree(geo_Prob);
	// mxFree(K_e);
	// mxFree(w_e);
	// mxFree(T_e);
	// mxFree(T_dot_e);
	// mxFree(Aeq);
	integer *n;
	integer *nclin;
	integer *lda;
	integer *ldh;
	doublereal a = 0.008721834027187;
	doublereal bl = -1 * std::numeric_limits<double>::infinity();
	doublereal bu = -3.080486520014580e-04;
	doublereal *cvec;
	doublereal *h__;
	S_fp qphess;
	integer *istate;
	doublereal *x;
	integer *inform__;
	integer *iter;
	doublereal *obj;
	doublereal *ax;
	doublereal *clamda;
	integer *iw;
	integer *leniw;
	doublereal *w; 
	integer *lenw;
	// cout << qpopt_(n, nclin, lda, ldh, a, bl, bu, cvec, h__, qphess, istate, x, 
	// inform__, iter, obj, ax, clamda, iw, leniw, w, lenw) << endl;
	integer i = 2;
	cout << qpopt(i) << endl;

}

// extern "C" int qpopt_(integer*, integer*, integer*, integer*, doublereal, doublereal, doublereal, doublereal,
// 	doublereal, S_fp, integer*, doublereal, integer*, integer*, doublereal, doublereal, 
// 	doublereal, integer*, integer*, doublereal, integer*); // not sure if pointers are necessary

AuxiliaryController::~AuxiliaryController() {
	
}

void AuxiliaryController::ComputeAuxiliaryController() {
	
}