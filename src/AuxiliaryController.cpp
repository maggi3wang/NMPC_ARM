/*
 * AuxiliaryController.cpp
 *
 *  Created on: July 18, 2016
 *      Author: maggiewang
 */

#include "AuxiliaryController.h"
#include <chrono>
#include <limits>
#include <stdio.h>   
#include <math.h> 

using namespace std::chrono;
using namespace f2c;

extern "C" int qpopt_
	(integer*, integer*, integer*, integer*,
	doublereal*, doublereal*, doublereal*, doublereal*,
	doublereal*, U_fp, integer*, doublereal*,
	integer*, integer*, doublereal*, doublereal*,
	doublereal*, integer*, integer*, doublereal*,
	integer*);

extern "C" int qphess_
	(integer*, integer*, integer*,
	doublereal*, doublereal*, doublereal*, integer*,
	integer*, doublereal*, integer*);

extern "C" int qpprmswrapper_
  (char*, integer*, ftnlen);

extern "C" int qpprms_
  (integer*, integer*);

extern "C" int qpprm_
  (char*, ftnlen);

extern "C" int qpprmi_
  (char*, integer*, ftnlen);


#define GRAV_CONST 9.81

AuxiliaryController::AuxiliaryController() {	
	u_prev = zeros(3, 1);
	att_des_prev = zeros(3, 1);
	om_des_prev = zeros(3, 1);
	Jx = 0.082; 
	Jy =  0.0845; 
	Jz =  0.1377;
	vec J;
	J << Jx << Jy << Jz;
	Jq = diagmat(J);

	a_const = (Jy-Jz)/Jx; 
	b_const = (Jz-Jx)/Jy; 
	c_const = (Jx-Jy)/Jz;

	mq = 4.34;
	kx = 16 * mq;
	kv = 5.6 * mq;
	dt = 0.005;
	// edit this later

	B_rot.insert_rows(0, zeros(3,3));
	B_rot.insert_rows(3, solve(Jq, eye(3, 3)));

	M_q.load("/Users/maggiewang/Workspace/QuadcoptersWorkspace/NMPC_ARM/src/M_q.dat", raw_ascii);
    state_nom.load("/Users/maggiewang/Workspace/QuadcoptersWorkspace/NMPC_ARM/src/state_nom.dat", raw_ascii);
    accel.load("/Users/maggiewang/Workspace/QuadcoptersWorkspace/NMPC_ARM/src/accel.dat", raw_ascii);
}	

AuxiliaryController::~AuxiliaryController() { }

void AuxiliaryController::PrintArray(int *ptr, size_t size) {
    int *const end = ptr + size;
    while( ptr < end ) {
        printf("%d ", *ptr++);        
    }
}

void AuxiliaryController::PrintArray(doublereal *ptr, size_t size) {
    doublereal *const end = ptr + size;
    while( ptr < end ) {
        printf("%f ", *ptr++);        
    }
}

void AuxiliaryController::PrintArray(integer *ptr, size_t size) {
    integer *const end = ptr + size;
    while( ptr < end ) {
        printf("%ld ", *ptr++);        
    }
}

// double AuxiliaryController::sec(double x) {
// 	return 1 / cos(x);
// }

// double AuxiliaryController::GetRoll(mat x) {
// 	return x(11) + x(12)*tan(x(9))*sin(x(8)) + x(13)*tan(x(9))*cos(x(8));
// }

// double AuxiliaryController::GetPitch(mat x) {
// 	return x(12)*cos(x(8)) - x(13)*sin(x(8));
// }

// double AuxiliaryController::GetYaw(mat x) {
// 	return x(12)*sec(x(9))*sin(x(8)) + x(13)*sec(x(9))*cos(x(8));
// }

// double AuxiliaryController::bq_1(mat x) {
// 	return sin(x(8))*sin(x(10)) + cos(x(8))*sin(x(9))*cos(x(10));
// }

// double AuxiliaryController::bq_2(mat x) {
// 	return -sin(x(8))*cos(x(10)) + cos(x(8))*sin(x(9))*sin(x(10));
// }

// double AuxiliaryController::bq_3(mat x) {
// 	return cos(x(8)) * cos(x(9));
// }

// mat AuxiliaryController::phi(mat x) {
// 	double g = 9.81;
// 	mat a;
// 	a 	<< x(0) << endr
// 		<< x(3)	<< endr
// 		<< -(1/mq)*x(6)*bq_1(x) << endr
// 		<< -(1/mq)*x(7)*bq_1(x)-(1/mq)*x(6)*(GetRoll(x)*cos(x(8))*sin(x(10))+GetYaw(x)*sin(x(8))*cos(x(10))-GetRoll(x)*sin(x(8))*sin(x(9))*cos(x(10))+GetPitch(x)*cos(x(8))*cos(x(9))*cos(x(10))-GetYaw(x)*cos(x(8))*sin(x(9))*sin(x(10))) << endr
//         << x(1)  << endr
//         << x(4)  << endr
//         << -(1/mq)*x(6)*bq_2(x)  << endr
//         << -(1/mq)*x(7)*bq_2(x)-(1/mq)*x(6)*(-GetRoll(x)*cos(x(8))*cos(x(10))+GetYaw(x)*sin(x(8))*sin(x(10))-GetRoll(x)*sin(x(8))*sin(x(9))*sin(x(10))+GetPitch(x)*cos(x(8))*cos(x(9))*sin(x(10))+GetYaw(x)*cos(x(8))*sin(x(9))*cos(x(10)))  << endr
//         << x(2)  << endr
//         << x(5)  << endr
//         << g - (1/mq) * x(6) * bq_3(x) << endr
//         << -(1/mq)*x(7)*bq_3(x)-(1/mq)*x(6)*(-GetRoll(x)*sin(x(8))*cos(x(9))-GetPitch(x)*cos(x(8))*sin(x(9))) << endr
//         << x(10) << endr
//         << GetYaw(x);

//     // cout << "disp1: " << g << endl;
//     // cout << "disp2: " << (1/mq) * x(6) * bq_3(x) << endl;
//     // cout << "disp3: " << g - (1/mq) * x(6) * bq_3(x) << endl;
//         // something is up with last four lines.
//     return a;
// }

// mat AuxiliaryController::phi_d(mat x) {
// 	mat a;
// 	a 	<< 1 << 0 << 0 <<  0 <<  0 <<  0 <<                                                                                   0 <<                                                   0 <<                                                                                                                     0 <<                                                                                0 <<                                                                                                                                                 0 <<                                                        0 <<                           0 <<                 0 << endr
// 		<< 0 << 0 <<  0 <<  1 <<  0 <<  0 <<                                                                                   0 <<                                                   0 <<                                                                                                                     0 <<                                                                                0 <<                                                                                                                                                 0 <<                                                        0 <<                           0 <<                 0 << endr
// 		<< 0 << 0 <<  0 <<  0 <<  0 <<  0 <<                                  -(sin(x(8))*sin(x(10)) + cos(x(8))*cos(x(10))*sin(x(9)))/mq <<                                                   0 <<                                                               -(x(6)*(cos(x(8))*sin(x(10)) - cos(x(10))*sin(x(8))*sin(x(9))))/mq <<                                               -(x(6)*cos(x(8))*cos(x(9))*cos(x(10)))/mq <<                                                                                           -(x(6)*(cos(x(10))*sin(x(8)) - cos(x(8))*sin(x(9))*sin(x(10))))/mq <<                                                        0 <<                           0 <<                 0 << endr
// 		<< 0 <<  0 <<  0 <<  0 <<  0 <<  0 <<  -(x(12)*cos(x(9))*cos(x(10)) + x(11)*cos(x(8))*sin(x(10)) - x(11)*cos(x(10))*sin(x(8))*sin(x(9)))/mq <<  -(sin(x(8))*sin(x(10)) + cos(x(8))*cos(x(10))*sin(x(9)))/mq <<  (x(6)*x(11)*sin(x(8))*sin(x(10)) - x(7)*cos(x(8))*sin(x(10)) + x(7)*cos(x(10))*sin(x(8))*sin(x(9)) + x(6)*x(11)*cos(x(8))*cos(x(10))*sin(x(9)))/mq <<  (cos(x(10))*(x(6)*x(12)*sin(x(9)) - x(7)*cos(x(8))*cos(x(9)) + x(6)*x(11)*cos(x(9))*sin(x(8))))/mq <<  -(x(7)*cos(x(10))*sin(x(8)) + x(6)*x(11)*cos(x(8))*cos(x(10)) - x(6)*x(12)*cos(x(9))*sin(x(10)) - x(7)*cos(x(8))*sin(x(9))*sin(x(10)) + x(6)*x(11)*sin(x(8))*sin(x(9))*sin(x(10)))/mq <<  -(x(6)*(cos(x(8))*sin(x(10)) - cos(x(10))*sin(x(8))*sin(x(9))))/mq <<  -(x(6)*cos(x(9))*cos(x(10)))/mq <<                 0 << endr
// 		<< 0 <<  1 <<  0 <<  0 <<  0 <<  0 <<                                                                                   0 <<                                                   0 <<                                                                                                                     0 <<                                                                                0 <<                                                                                                                                                 0 <<                                                        0 <<                           0 <<                 0 << endr
// 		<< 0 <<  0 <<  0 <<  0 <<  1 <<  0 <<                                                                                   0 <<                                                   0 <<                                                                                                                     0 <<                                                                                0 <<                                                                                                                                                 0 <<                                                        0 <<                           0 <<                 0 << endr
// 		<< 0 <<  0 <<  0 <<  0 <<  0 <<  0 <<                                   (cos(x(10))*sin(x(8)) - cos(x(8))*sin(x(9))*sin(x(10)))/mq <<                                                   0 <<                                                                (x(6)*(cos(x(8))*cos(x(10)) + sin(x(8))*sin(x(9))*sin(x(10))))/mq <<                                               -(x(6)*cos(x(8))*cos(x(9))*sin(x(10)))/mq <<                                                                                           -(x(6)*(sin(x(8))*sin(x(10)) + cos(x(8))*cos(x(10))*sin(x(9))))/mq <<                                                        0 <<                           0 <<                 0 << endr
// 		<< 0 <<  0 <<  0 <<  0 <<  0 <<  0 <<   (x(11)*cos(x(8))*cos(x(10)) - x(12)*cos(x(9))*sin(x(10)) + x(11)*sin(x(8))*sin(x(9))*sin(x(10)))/mq <<   (cos(x(10))*sin(x(8)) - cos(x(8))*sin(x(9))*sin(x(10)))/mq <<  (x(7)*cos(x(8))*cos(x(10)) - x(6)*x(11)*cos(x(10))*sin(x(8)) + x(7)*sin(x(8))*sin(x(9))*sin(x(10)) + x(6)*x(11)*cos(x(8))*sin(x(9))*sin(x(10)))/mq <<  (sin(x(10))*(x(6)*x(12)*sin(x(9)) - x(7)*cos(x(8))*cos(x(9)) + x(6)*x(11)*cos(x(9))*sin(x(8))))/mq <<  -(x(7)*sin(x(8))*sin(x(10)) + x(6)*x(12)*cos(x(9))*cos(x(10)) + x(6)*x(11)*cos(x(8))*sin(x(10)) + x(7)*cos(x(8))*cos(x(10))*sin(x(9)) - x(6)*x(11)*cos(x(10))*sin(x(8))*sin(x(9)))/mq <<   (x(6)*(cos(x(8))*cos(x(10)) + sin(x(8))*sin(x(9))*sin(x(10))))/mq <<  -(x(6)*cos(x(9))*sin(x(10)))/mq <<                 0 << endr
// 		<< 0 <<  0 <<  1 <<  0 <<  0 <<  0 <<                                                                                   0 <<                                                   0 <<                                                                                                                     0 <<                                                                                0 <<                                                                                                                                                 0 <<                                                        0 <<                           0 <<                 0 << endr
// 		<< 0 <<  0 <<  0 <<  0 <<  0 <<  1 <<                                                                                   0 <<                                                   0 <<                                                                                                                     0 <<                                                                                0 <<                                                                                                                                                 0 <<                                                        0 <<                           0 <<                 0 << endr
// 		<< 0 <<  0 <<  0 <<  0 <<  0 <<  0 <<                                                              -(cos(x(8))*cos(x(9)))/mq <<                                                   0 <<                                                                                              (x(6)*cos(x(9))*sin(x(8)))/mq <<                                                         (x(6)*cos(x(8))*sin(x(9)))/mq <<                                                                                                                                                 0 <<                                                        0 <<                           0 <<                 0 << endr
// 		<< 0 <<  0 <<  0 <<  0 <<  0 <<  0 <<                                            (x(12)*sin(x(9)) + x(11)*cos(x(9))*sin(x(8)))/mq <<                              -(cos(x(8))*cos(x(9)))/mq <<                                                                           (cos(x(9))*(x(7)*sin(x(8)) + x(6)*x(11)*cos(x(8))))/mq <<             (x(6)*x(12)*cos(x(9)) + x(7)*cos(x(8))*sin(x(9)) - x(6)*x(11)*sin(x(8))*sin(x(9)))/mq <<                                                                                                                                                 0 <<                                 (x(6)*cos(x(9))*sin(x(8)))/mq <<            (x(6)*sin(x(9)))/mq <<                 0 << endr
// 		<< 0 <<  0 <<  0 <<  0 <<  0 <<  0 <<                                                                                   0 <<                                                   0 <<                                                                                                                     0 <<                                                                                0 <<                                                                                                                                                 1 <<                                                        0 <<                           0 <<                 0 << endr
// 		<< 0 <<  0 <<  0 <<  0 <<  0 <<  0 <<                                                                                   0 <<                                                   0 <<                                                                                  (x(12)*cos(x(8)) - x(13)*sin(x(8)))/cos(x(9)) <<              (sin(x(9))*(x(13)*(2*sin(x(8)/2)*sin(x(8)/2) - 1) - x(12)*sin(x(8))))/((sin(x(9))) * (sin(x(9))) - 1) <<                                                                                                                                                 0 <<                                                        0 <<            sin(x(8))/cos(x(9)) <<  cos(x(8))/cos(x(9));
// 	return a;
// }

// mat AuxiliaryController::M(mat x) {
// 	mat a;
// 	a = M_ccm * phi_d(x);
// 	return a;
// }

// mat AuxiliaryController::f(mat x) {
// 	double a = -0.648780487804878;
// 	double b = 0.659171597633136;
// 	double c = -0.0181554103122731;
// 	double g = 9.81;
// 	mat y;
// 	y << x(3) << endr
//         <<  x(4) << endr
//         <<  x(5) << endr
//         <<  -(1/mq)*x(6)*bq_1(x) << endr
//         <<  -(1/mq)*x(6)*bq_2(x) << endr
//         <<  g - (1/mq)*x(6)*bq_3(x) << endr
//         <<  x(7) << endr
//         <<  0 << endr
//         <<  GetRoll(x) << endr
//         <<  GetPitch(x) << endr
//         <<  GetYaw(x) << endr
//         <<  a*x(12)*x(13) << endr
//         <<  b*x(11)*x(13) << endr
//         <<  c*x(11)*x(12);
// 	return y;
// }

mat AuxiliaryController::RotationMatrix(double z, double y, double x) {
	// body to internal
	// order z, y, x
	mat Rz;
	Rz << cos(z)  << sin(z) << 0 << endr
	   << -sin(z) << cos(z) << 0 << endr
	   << 0		  << 0		<< 1;

	mat Ry;
	Ry << cos(y) << 0 << -sin(y) << endr
	   << 0      << 1 << 0       << endr
	   << sin(y) << 0 << cos(y);

	mat Rx;
	Rx << 1		<< 0		<< 0 	  << endr
	   << 0		<< cos(x)   << sin(x) << endr
	   << 0		<< -sin(x)  << cos(x);

	return Rz.t() * Ry.t() * Rx.t();
}

mat AuxiliaryController::R_om(mat x) {
	mat a;
	a 	<< 1 << 0 		   << -sin(x(1)) 			<< endr
		<< 0 << cos(x(0))  << sin(x(0))*cos(x(1))  << endr
		<< 0 << -sin(x(0)) << cos(x(0))*cos(x(1));
	return a;
}

mat AuxiliaryController::phi_d(mat x) {
	mat a;
	a.insert_cols(0, eye(3, 3));
	a.insert_cols(3, zeros(3, 3));

	mat temp;
	temp << 0 << -x(5)*cos(x(1)) << 0 << endr
		 << -x(4)*sin(x(0))+x(5)*cos(x(0))*cos(x(1)) << -x(5)*sin(x(0))*sin(x(1)) << 0 << endr
		 <<  -x(4)*cos(x(0))-x(5)*sin(x(0))*cos(x(1)) << -x(5)*cos(x(0))*sin(x(1)) << 0;
	temp.insert_cols(3, R_om(x));

	a.insert_rows(3, temp);
	return a;
}

mat AuxiliaryController::M(mat x) {
	return M_q * solve(phi_d(x), eye(6, 6));
}

double AuxiliaryController::sec(double x) {
	return 1 / cos(x);
}

double AuxiliaryController::GetRoll(mat x) {
	return x(9) + x(10)*tan(x(7))*sin(x(6)) + x(11)*tan(x(7))*cos(x(6));
}

double AuxiliaryController::GetPitch(mat x) {
	return x(10)*cos(x(6)) - x(11)*sin(x(6));
}

double AuxiliaryController::GetYaw(mat x) {
	return x(10)*sec(x(7))*sin(x(6)) + x(11)*sec(x(7))*cos(x(6));
}

double AuxiliaryController::bq_1(mat x) {
	return sin(x(6))*sin(x(8)) + cos(x(6))*sin(x(7))*cos(x(8));
}

double AuxiliaryController::bq_2(mat x) {
	return -sin(x(6))*cos(x(8)) + cos(x(6))*sin(x(7))*sin(x(8));
}

double AuxiliaryController::bq_3(mat x) {
	return cos(x(6))*cos(x(7));
}

mat AuxiliaryController::f(mat x) {
	mat y;
	y << x(3) << endr
      << x(4)<< endr
      << x(5)<< endr
      << 0 << endr
      << 0 << endr
      << GRAV_CONST << endr
      << GetRoll(x) << endr
      << GetPitch(x) << endr
      << GetYaw(x) << endr
      << a_const*x(10)*x(11) << endr
      << b_const*x(9)*x(11) << endr
      << c_const*x(9)*x(10);
    return y;
}
     
mat AuxiliaryController::f_rot(mat x) {
	mat a;
	a << x(3) + x(4)*tan(x(1))*sin(x(0)) + x(5)*tan(x(1))*cos(x(0)) << endr
       <<       x(4)*cos(x(0)) - x(5)*sin(x(0)) << endr
       <<       x(4)*sec(x(1))*sin(x(0)) + x(5)*sec(x(1))*cos(x(0)) << endr
       <<       a_const*x(4)*x(5) << endr
       <<       b_const*x(3)*x(5) << endr
       <<       c_const*x(3)*x(4);
    return a;
} 

// make sure xi_act is a column
void AuxiliaryController::ComputeAuxiliaryController(mat x_nom, mat u_nom, mat x_act, mat ang_a, mat a_des, int iteration) {	// will get state_nom and ctr_nom from a file
	high_resolution_clock::time_point t1 = high_resolution_clock::now();

	double eps_u = 0.0;		// move later

	cout.precision(16);
	cout.setf(ios::fixed);
	
	mat ex = x_act(span(0, 2), 0) - x_nom(span(0, 2), 0);
	mat ev = x_act(span(3, 5), 0) - x_nom(span(3, 5), 0);

	mat temp1;
	temp1 << 0 << endr
		  << 0 << endr
		  << 1;

	mat thrust_des = -kx * ex - kv * ev - mq * GRAV_CONST * temp1 + mq * a_des;

	mat temp2;
	temp2 << 0  << endr
		  << 0  << endr
		  << -1;

	mat R = RotationMatrix(x_act(8), x_act(7), x_act(6));
	mat thrust = thrust_des.t() * (R * temp2);

	mat zb_des = -thrust_des / norm(thrust_des);
	double yaw_des = x_nom(8);
	double pitch_des = atan2(zb_des(0)*cos(yaw_des) + zb_des(1)*sin(yaw_des), zb_des(2));
	double roll_des = atan2(zb_des(0)*sin(yaw_des) - zb_des(1)*cos(yaw_des),
                   zb_des(2)/cos(pitch_des));

	mat att_des;
	att_des << roll_des   << endr
	        << pitch_des << endr
	        << yaw_des;

	//         // for now
	// att_des_prev << -0.241884148822710 << endr
	// << -0.114373921475904 << endr
	// << 0;

 //    om_des_prev
	mat rate_eul_des;
	mat om_des;
	mat torque_nom;

    if (iteration == 0) {
    	mat temp3;
    	temp3 << 0 << endr << 0 << endr << GRAV_CONST;

    	mat temp4;
    	temp4 << bq_1(x_act) << endr << bq_2(x_act) << endr << bq_3(x_act) << endr;

    	mat a_pred = temp3 + (-1/mq) * temp4 * thrust;
    	// cout << "\na_pred: " << endl;
    	// a_pred.print();
        
        mat temp5;
        temp5.insert_cols(0, x_act(span(0, 2), 0));
        temp5.insert_rows(3, x_act(span(3, 5), 0));

        mat temp6;
        temp6.insert_cols(0, x_act(span(3,5), 0));
        temp6.insert_rows(3, a_pred);

        mat xv_pred = temp5 + dt * temp6;

        mat ex_pred = xv_pred(span(0, 2), 0) - state_nom(1, span(0, 2)).t();
        mat ev_pred = xv_pred(span(3, 5), 0) - state_nom(1, span(3, 5)).t();

        // ex_pred.print();
        // cout <<"-----" << endl;
        // ev_pred.print();

        mat a_des = accel.row(1).t();
        mat thrust_des_pred = -kx * ex_pred - kv * ev_pred - mq * GRAV_CONST * temp1 + mq * a_des;

        mat zb_des_pred = -thrust_des_pred / norm(thrust_des_pred);
        
        double yaw_des_pred = state_nom(1, 8);
        double pitch_des_pred = atan2(zb_des_pred(0) * cos(yaw_des_pred) + zb_des_pred(1) * sin(yaw_des_pred), zb_des_pred(2));
        double roll_des_pred = atan2(zb_des_pred(0) * sin(yaw_des_pred) - zb_des_pred(1) * cos(yaw_des_pred), zb_des_pred(2)/cos(pitch_des_pred));
    	
    	mat att_des_pred;
    	att_des_pred << roll_des_pred << endr << pitch_des_pred << endr << yaw_des_pred;

    	rate_eul_des = (att_des_pred - att_des) / dt;
        om_des = R_om(att_des) * rate_eul_des;

        mat R_nom = RotationMatrix(x_nom(8),x_nom(7),x_nom(6));
        mat R_des = RotationMatrix(att_des(2),att_des(1),att_des(0));
        mat ang_accel_des = R_des.t() * R_nom * ang_a;	// may change ang_a later
        torque_nom = Jq * ang_accel_des + cross(om_des,Jq * om_des);
        
        att_des_prev = att_des;
        om_des_prev = om_des;
    } else {
		rate_eul_des = (att_des - att_des_prev)/dt;
		om_des = R_om(att_des)*rate_eul_des;
		mat ang_accel_des = (om_des - om_des_prev)/dt;
		torque_nom = Jq*ang_accel_des + cross(om_des,Jq*om_des);

		att_des_prev = att_des;
		om_des_prev = om_des;
    }

    mat rate_eul_act = solve(R_om(x_act(span(6, 8), 0)), x_act(span(9, 11), 0));
    
    mat q_des;
    q_des.insert_cols(0, att_des);
    q_des.insert_rows(3, rate_eul_des);

    mat q_act;
    q_act.insert_cols(0, x_act(span(6, 8), 0));
    q_act.insert_rows(3, rate_eul_act);

    mat xi_q_des;
    xi_q_des.insert_cols(0, att_des);
    xi_q_des.insert_rows(3, om_des);

    mat xi_q_act;
    xi_q_act.insert_cols(0, x_act(span(6, 8), 0));
    xi_q_act.insert_rows(3, x_act(span(9, 11), 0));

    mat Xq_dot = kron(ones(1, 2), q_act - q_des);
    
    mat X_xi;
    X_xi.insert_cols(0, xi_q_des);
    X_xi.insert_cols(1, xi_q_act);

    mat X_q;
    X_q.insert_cols(0, q_des);
    X_q.insert_cols(1, q_act);

    mat e = (q_act - q_des).t() * M_q * (q_act - q_des);
    double E = e(0);

   /////////////////////////////////////////////////////////////////////////////////////////////
    
    // n (>0) is n, the number of variables in the problem
    integer num_vars = 3;
    
    // nclin (>= 0) is m_L, the number of general linear constraints
    integer nclin = 1;
    
    // ldA (>= 1 and >= nclin) is the row dimension for array A
    integer ldA = 1;
    
    // ldH (>=1 and >= n) is the row dimension of array H.
    // (ldH must be at least the value of Hessian Rows if that parameter is set.)
    integer ldH = 3;
    
    // A is an array of dimension (ldA,k) for some k ≥ n.
    // It contains the matrix A for the linear constraints.
    // If nclin is zero, A is not referenced.
    // (In that case, A may be dimensioned (ldA,1) with ldA = 1, or it could be any convenient array.)
	int k = 2;
	mat A = 2 * Xq_dot.col(k-1).t() * (M(X_q.col(k-1)) * B_rot);
	doublereal A_arr[3] = { A(0), A(1), A(2) };
	doublereal *A_ptr = A_arr;
	cout << "A: ";
	A.print();
	cout << endl;

    // bl is an array of dimension at least n + nclin containing the lower bounds l in prob- lem LCQP.
    // To specify a non-existent bound (lj = −∞), set bl(j) ≤ −bigbnd,
    // where bigbnd is the Infinite Bound (default value 1020).
    // To specify an equality constraint rj(x) = β, set bl(j) = bu(j) = β, where |β| < bigbnd.
    double infinity = std::numeric_limits<double>::infinity();
    doublereal bl[4] = { -infinity, -infinity, -infinity, -infinity };
    doublereal *bl_ptr = bl;
    
    // bu is an array of dimension at least n+nclin containing the upper bounds u in problem LCQP.
    // To specify a non-existent bound (uj = ∞), set bu(j) ≥ bigbnd.
    // The bounds must satisfy bl(j) ≤ bu(j) for all j.
    // double lambda = 2.4;
    // mat u_b_ofA = -2 * lambda * E + 2 * X_dot.col(0).t() * M(X.col(0)) * (f(X.col(0)) + B * u_nom) - 2 * X_dot.col(k-1).t() * M(X.col(k-1)) * (f(X.col(k-1)) + B*u_nom);
    // double u_b_ofA_d = u_b_ofA(0);
    double lambda = 1.0;
	mat u_b_ofA = -2 * lambda * E + 2 * Xq_dot.col(0).t() * (M(X_q.col(0)) * (f_rot(X_xi.col(0)) + B_rot * torque_nom)) 
	              - 2 * Xq_dot.col(k-1).t() * (M(X_q.col(k-1)) * (f_rot(X_xi.col(k-1)) + B_rot*torque_nom));
	
	double u_b_ofA_d = u_b_ofA(0);

    doublereal bu[4] = { infinity, infinity, infinity, u_b_ofA_d };
    doublereal *bu_ptr = bu;
    
    // cvec is an array of dimension at least n that contains the explicit linear term c of the objective.
    // If the problem is of type FP, QP1, or QP3, cvec is not referenced.
    // (In that case, cvec may be dimensioned (1), or it could be any convenient array.)
    //int m = 2;        // move this
    
    // mat cvec_mat = (2 * eps_u) * (u_nom - u_prev);
    // cout << "cvec_mat:" << endl;
    // cvec_mat.print();
    // cout << endl;
    mat cvec_mat = ((2 * eps_u)) * (torque_nom - u_prev);
    doublereal cvec[3] = { cvec_mat(0), cvec_mat(1), cvec_mat(2) };    // in NMPC_cont, cvec is [0; 0] --> maybe make into column?
    doublereal *cvec_ptr = cvec;
    
    //  is an array of dimension (ldH,k) for some k ≥ n.
    // H may be used to store the matrix H associated with the quadratic term of the QP objective.
    // It is not referenced if the problem type is FP or LP.
    // (In that case, H may be dimensioned (ldH,1) with ldH = 1,
    // or it could be any convenient array.)
    doublereal H[9] = { 1+eps_u, 0.0, 0.0, 
                       0, 1+eps_u, 0,
                       0, 0, 1+eps_u };    // edited
    
    //(1+eps_u)*eye(m)
    doublereal *H_ptr = H;
    
    /*
     qphess is the name of a subroutine that defines the product Hx for a given vector x.
     It must be declared as external in the routine that calls qpopt.
     In general, the user need not provide this parameter,
     because a “default” subroutine named qpHess is distributed with QPOPT.
     It uses the array H defined above.
     In some cases, a specialized routine may be desirable.
     For a detailed description of qpHess, see Section 5.
     */
    
    // no declaration for now
    integer istate;
    
    // x is an array of dimension at least n.
    // It contains an initial estimate of the solution.
    doublereal x[3] = { 0.0, 0.0, 0.0 };
    doublereal *x_ptr = x;
    
    ////// on exit //////
    integer inform = 0;
    integer iter = 0;
    doublereal obj = 0;
    
    doublereal Ax[3] = { 0.0, 0.0, 0.0 };
    doublereal *Ax_ptr = Ax;
    
    doublereal clambda[4] = { 0.0, 0.0, 0.0, 0.0 };
    doublereal *clambda_ptr = clambda;
    /////////////////////
    
    // iw is an array of dimension leniw that provides integer workspace.
    integer iw[9] = { 0, 0, 0, 0, 0,
                      0, 0, 0, 0 };
    integer *iw_ptr = iw;
    
    // leniw is the dimension of iw. It must be at least 2 n + 3.
    integer leniw = 9;
    
    // w is an array of dimension lenw that provides real workspace.
    doublereal w[47] = { 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,
        0, 0 };
    doublereal *w_ptr = w;
    
    // lenw is the dimension of w.
    // It depends on the Problem type and nclin as shown in the following table.
    integer lenw = 47;
    
    cout << qpopt_ ( &num_vars, &nclin, &ldA, &ldH, A_ptr, bl_ptr, bu_ptr, cvec_ptr, H_ptr, (U_fp)qphess_,
                    &istate, x_ptr, &inform, &iter, &obj, Ax_ptr, clambda, iw_ptr, &leniw, w_ptr, &lenw ) << endl;
    
    /////////////////////////////////////////////////////////////////////////////////////////////

	mat aux = zeros(3, 1);
    aux(0) = *x_ptr++;       
    aux(1) = *x_ptr++;
    aux(2) = *x_ptr++;
    cout << "aux: " << endl;
    aux.print();
    cout << endl;

    cout << "u_nom: " << endl;
    u_nom.print();
    cout << endl;

    //aux_torque.insert_rows(iteration, aux);
    mat ctrl_add;
    ctrl_add.insert_rows(0, thrust);
    ctrl_add.insert_cols(1, torque_nom.t() + aux.t());

    cout << "ctrl_add:" <<endl;
    ctrl_add.print();
    
    ctrl.insert_rows(iteration, ctrl_add);
	u_prev = ctrl(iteration, span(1, 3)).t();   
	cout << "u_prev" << endl;
	u_prev.print();

	// mat temp_w_dist;
	// temp_w_dist << 0 << endr << 0.5 << endr << 0.5 << endr << 0.5;
	// mat w_dist = u_nom % temp_w_dist;

	//x_act.row(iteration+1) = x_act.t() + (f(x_act) + B(x_act)*ctrl.row(iteration).t() + B_w(x_act)*w_dist).t() * dt;
    // u_prev = aux.t() + u_nom;
    // cout << "u_prev:" << endl;
    // u_prev.print();

	// cout << "obj: " << obj << endl;

	high_resolution_clock::time_point t2 = high_resolution_clock::now();
	double duration = duration_cast<microseconds>( t2 - t1 ).count();
	cout << "duration: " << duration << endl;
	cout << "-----------------------------\n\n" << endl;

// //	mat w_dist = u_nom.%[0;0.1*ones(3,1)];
// 	// x_act

}
