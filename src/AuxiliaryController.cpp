/*
 * AuxiliaryController.cpp
 *
 *  Created on: July 18, 2016
 *      Author: maggiewang
 */

#include "AuxiliaryController.h"
#include <chrono>
#include <limits>

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

AuxiliaryController::AuxiliaryController() {	
	M_ccm << 96.3076919922660 		<<	18.4615391668441 		<< 	-3.36877394563014e-19	<< 8.08505152267912e-19 << endr
		  << 18.4615391668441 		<< 	7.69230798175400		<< 8.08505149725934e-19		<< 3.36877156327604e-19 << endr
		  << -3.36877394563013e-19	<<  8.08505149725934e-19	<< 96.3076919922727			<< 18.4615391668281 	<< endr 
		  << 8.08505152267912e-19	<<  3.36877156327604e-19 	<< 18.4615391668281			<< 7.69230798174735;

	W_ccm << 0.0192307699828708	 	<< -0.0461538479854762 		<< 	8.42193001858584e-22 	<<	-2.02126314716476e-21 	<< endr 
		  << -0.0461538479854762 	<<	0.240769230337307		<< -2.02126314080981e-21	<< -8.42192113058438e-22 	<< endr 
		  << 8.42193001858584e-22	<< -2.02126314080981e-21	<< 0.0192307699828542		<< -0.0461538479854362 		<< endr 
		  << -2.02126314716476e-21	<< -8.42192113058438e-22	<< -0.0461538479854362 		<<	0.240769230337324;
	u_prev = zeros(2, 1);		// make 2 a variable (m)
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

mat AuxiliaryController::phi(mat x) {
	mat a;
	a << x(0)			  << endr
	  << x(2) * cos(x(3)) << endr
	  << x(1)			  << endr
	  << x(2) * sin(x(3));
	return a;
}

mat AuxiliaryController::phi_d(mat x) {
	mat a;
	a << 1 << 0 << 0          << 0               << endr
      << 0 << 0 << cos(x(3))  << -x(2)*sin(x(3)) << endr
      << 0 << 1 << 0          << 0               << endr             
      << 0 << 0 << sin(x(3))  << x(2)*cos(x(3));
    return a;
}

// mat AuxiliaryController::geo_map(mat x_start, mat x_end, mat xi_start, mat xi_end, int n) {
// 	mat xi_dot = xi_end - xi_start;
// 	mat X_dot = zeros(n, 2);
// 	X_dot.col(0) = solve(phi_d(x_start), xi_dot);
// 	X_dot.col(1) = solve(phi_d(x_end), xi_dot);
// 	return X_dot;
// }

// mat AuxiliaryController::W(mat x, int n) {
// 	mat a;
// 	a = solve(phi_d(x), W_ccm) * solve(phi_d(x), eye(n, n)).t();
// 	return a;
// }

mat AuxiliaryController::M(mat x) {
	mat a;
	a = M_ccm * phi_d(x);
	return a;
}

mat AuxiliaryController::f(mat x) {
	mat a;
	a << x(2) * cos(x(3)) << endr
	  << x(2) * sin(x(3)) << endr
	  << 0				  << endr
	  << 0				  << endr;
	return a;
}

// make sure xi_act is a column
void AuxiliaryController::ComputeAuxiliaryController(double x_d, double y_d, double v_d, double theta_d, double a_d, double om_d, mat x_act) {		
	high_resolution_clock::time_point t1 = high_resolution_clock::now();
	double eps_u = 0.2;

	// in future, will read x_nom and u_nom from a file
	mat x_nom;
	x_nom << x_d << endr << y_d << endr << v_d << endr << theta_d;
	
	mat u_nom;
	u_nom << a_d << endr << om_d;
	//
	
	mat xi_nom = phi(x_nom);
	mat xi_act = phi(x_act);

	int num = 4;	// modify later
	// mat X_dot = geo_map(x_nom, x_act, xi_nom, xi_act, num);
	mat X_dot = kron(ones(1,2), xi_act - xi_nom);

	mat X = zeros(4, 2);	// modify later
	X.col(0) = x_nom;
	X.col(1) = x_act;

	mat e = (((xi_act - xi_nom).t() * M_ccm) * (xi_act - xi_nom));
	double E = e(0);

	mat B;
	B << 0 << 0 << endr << 0 << 0 << endr << 1 << 0 << endr << 0 << 1;

	/////////////////////////////////////////////////////////////////////////////////////////////

	// n (>0) is n, the number of variables in the problem
	integer n = 2;

	// nclin (>= 0) is m_L, the number of general linear constraints
	integer nclin = 1;

	// ldA (>= 1 and >= nclin) is the row dimension for array A
	integer ldA = 1;	

	// ldH (>=1 and >= n) is the row dimension of array H. 
	// (ldH must be at least the value of Hessian Rows if that parameter is set.)
	integer ldH = 2;	
	
	// A is an array of dimension (ldA,k) for some k ≥ n. 
	// It contains the matrix A for the linear constraints. 
	// If nclin is zero, A is not referenced. 
	// (In that case, A may be dimensioned (ldA,1) with ldA = 1, or it could be any convenient array.)
	int k = 2;
	mat A = 2 * X_dot.col(k-1).t() * M(X.col(k-1)) * B;
	doublereal A_arr[2] = { A(0), A(1) };
	doublereal *A_ptr = A_arr;
	cout << "A: ";
	A.print();
	cout << endl;
	
	// bl is an array of dimension at least n + nclin containing the lower bounds l in prob- lem LCQP. 
	// To specify a non-existent bound (lj = −∞), set bl(j) ≤ −bigbnd, 
	// where bigbnd is the Infinite Bound (default value 1020). 
	// To specify an equality constraint rj(x) = β, set bl(j) = bu(j) = β, where |β| < bigbnd.
	double infinity = std::numeric_limits<double>::infinity();
	doublereal bl[4] = { -infinity, -infinity, -infinity };	
	doublereal *bl_ptr = bl;

	// bu is an array of dimension at least n+nclin containing the upper bounds u in problem LCQP. 
	// To specify a non-existent bound (uj = ∞), set bu(j) ≥ bigbnd. 
	// The bounds must satisfy bl(j) ≤ bu(j) for all j.
	double lambda = 2.4;
	mat u_b_ofA = -2 * lambda * E + 2 * X_dot.col(0).t() * M(X.col(0)) * (f(X.col(0)) + B * u_nom) - 2 * X_dot.col(k-1).t() * M(X.col(k-1)) * (f(X.col(k-1)) + B*u_nom);
	double u_b_ofA_d = u_b_ofA(0);
	doublereal bu[4] = { infinity, infinity, u_b_ofA_d }; 
	doublereal *bu_ptr = bu;
	cout << "u_b_ofA" << u_b_ofA << endl;

	// cvec is an array of dimension at least n that contains the explicit linear term c of the objective. 
	// If the problem is of type FP, QP1, or QP3, cvec is not referenced. 
	// (In that case, cvec may be dimensioned (1), or it could be any convenient array.)
	int m = 2;		// move this

	mat cvec_mat = (2 * eps_u) * (u_nom - u_prev);
	cout << "cvec_mat:" << endl;
	cvec_mat.print();
	cout << endl;
	doublereal cvec[2] = { cvec_mat(0), cvec_mat(1) };	// in NMPC_cont, cvec is [0; 0] --> maybe make into column?
	doublereal *cvec_ptr = cvec;

	//  is an array of dimension (ldH,k) for some k ≥ n. 
	// H may be used to store the matrix H associated with the quadratic term of the QP objective. 
	// It is not referenced if the problem type is FP or LP. 
	// (In that case, H may be dimensioned (ldH,1) with ldH = 1, 
	// or it could be any convenient array.)	
	doublereal H[4] = { 1+eps_u, 0.0, 0.0, 1+eps_u };	// edited

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
	doublereal x[2] = { 0.0, 0.0 };
	doublereal *x_ptr = x;

	////// on exit //////
	integer inform = 0;
	integer iter = 0;
	doublereal obj = 0;

	doublereal Ax[2] = { 0.0, 0.0 };
    doublereal *Ax_ptr = Ax;

	doublereal clambda[3] = { 0.0, 0.0, 0.0 };
	doublereal *clambda_ptr = clambda;
	/////////////////////

	// iw is an array of dimension leniw that provides integer workspace.
	integer iw[7] = { 0, 0, 0, 0, 0,
					  0, 0 };
	integer *iw_ptr = iw;

	// leniw is the dimension of iw. It must be at least 2 n + 3.
	integer leniw = 7;

	// w is an array of dimension lenw that provides real workspace.
	doublereal w[34] = { 0, 0, 0, 0, 0,
						 0, 0, 0, 0, 0, 
						 0, 0, 0, 0, 0,
						 0, 0, 0, 0, 0,
						 0, 0, 0, 0, 0,
						 0, 0, 0, 0, 0,
						 0, 0, 0, 0 };
	doublereal *w_ptr = w;

	// lenw is the dimension of w. 
	// It depends on the Problem type and nclin as shown in the following table.
	integer lenw = 34;

	cout << qpopt_ ( &n, &nclin, &ldA, &ldH, A_ptr, bl_ptr, bu_ptr, cvec_ptr, H_ptr, (U_fp)qphess_,
	&istate, x_ptr, &inform, &iter, &obj, Ax_ptr, clambda, iw_ptr, &leniw, w_ptr, &lenw ) << endl;

	/////////////////////////////////////////////////////////////////////////////////////////////

	mat aux = zeros(1, 2);
    aux(0) = *x_ptr++;       
    aux(1) = *x_ptr++;
    cout << "aux: " << endl;
    aux.print();
    cout << endl;

    cout << "u_nom" << endl;
    u_nom.print();
    cout << endl;

    u_prev = aux.t() + u_nom;
    cout << "u_prev:" << endl;
    u_prev.print();

	cout << "obj: " << obj << endl;

	high_resolution_clock::time_point t2 = high_resolution_clock::now();
	double duration = duration_cast<microseconds>( t2 - t1 ).count();
	cout << "duration: " << duration << endl;
	cout << "-----------------------------\n\n" << endl;

}
