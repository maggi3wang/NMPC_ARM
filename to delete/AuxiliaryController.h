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
//#include <armadillo>

using namespace std;
// using namespace arma;

class AuxiliaryController {

public:
	AuxiliaryController();
	virtual ~AuxiliaryController();
	void ComputeAuxiliaryController();

private:

};

#endif /* AUXILIARYCONTROLLER_H_ */