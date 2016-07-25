/*
 * AuxiliaryController.h
 *
 *  Created on: July 18, 2016
 *      Author: maggiewang
 */

#ifndef AUXILIARYCONTROLLER_H_
#define AUXILIARYCONTROLLER_H_

#include <math.h>
#include <vector>
#include <iostream>
#include <string>
#include <sstream>
#include <tuple>
#include <armadillo>
#include "f2c.h"

using namespace std;
using namespace arma;
using namespace f2c;

class AuxiliaryController {

public:
	AuxiliaryController();
	virtual ~AuxiliaryController();
	void ComputeAuxiliaryController(double x_d, double y_d, double v_d, double theta_d, double a_d, double om_d, mat x_act);
private:
	void PrintArray(int *ptr, size_t size);
	void PrintArray(doublereal *ptr, size_t size);
	void PrintArray(integer *ptr, size_t size);
	mat phi(mat x);
	mat phi_d(mat x);
	mat geo_map(mat x_start, mat x_end, mat xi_start, mat xi_end, int n);
	mat M(mat x);
	mat f(mat x);

	mat M_ccm;
	mat W_ccm;
	mat u_prev;
};

#endif /* AUXILIARYCONTROLLER_H_ */