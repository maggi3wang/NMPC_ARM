///*
// * AuxiliaryController.h
// *
// *  Created on: July 18, 2016
// *      Author: maggiewang
// */
//
//#ifndef AUXILIARYCONTROLLER_H_
//#define AUXILIARYCONTROLLER_H_
//
//#include <math.h>
//#include <vector>
//#include <iostream>
//#include <string>
//#include <sstream>
//#include <tuple>
//#include <armadillo>
//#include "f2c.h"
//
//using namespace std;
//using namespace arma;
//using namespace f2c;
//
//class AuxiliaryController {
//
//public:
//	AuxiliaryController();
//	virtual ~AuxiliaryController();
//	//void ComputeAuxiliaryController(double x_d, double y_d, double v_d, double theta_d, double a_d, double om_d, mat x_act);
//	void ComputeAuxiliaryController(mat x_nom, mat u_nom, mat x_act, mat ang_a, mat a_des, int iteration);
//
//private:
//	void PrintArray(int *ptr, size_t size);
//	void PrintArray(doublereal *ptr, size_t size);
//	void PrintArray(integer *ptr, size_t size);
//
//	mat RotationMatrix(double z, double y, double x);
//	mat R_om(mat x);
//	mat phi_d(mat x);
//	mat M(mat x);
//	double sec(double x);
//	double GetRoll(mat x);
//	double GetPitch(mat x);
//	double GetYaw(mat x);
//	double bq_1(mat x);
//	double bq_2(mat x);
//	double bq_3(mat x);
//	mat f(mat x);
//	mat f_rot(mat x);
//
//	mat att_des_prev;
//	mat om_des_prev;
//
//	double Jx, Jy, Jz;
//	mat Jq;
//	mat u_prev;
//
//	double a_const, b_const, c_const;	// MAKE THESE CONSTS!
//	double mq;
//	double kx;
//	double kv;
//	double dt;
//	mat state_nom, accel;
//	mat M_q;
//	mat B_rot;
//	mat aux_torque;
//	mat ctrl;
//};
//
//#endif /* AUXILIARYCONTROLLER_H_ */