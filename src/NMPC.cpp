//#include "AuxiliaryController.h"
//#include <stdio.h>
//#include <armadillo>
#include "f2c.h"
//
//using namespace std;
//using namespace arma;
using namespace f2c;
//

/* for qpopt */

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

int main() {
     // testing
    
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
     //     int k = 2;
     //     mat A = 2 * Xq_dot.col(k-1).t() * (M(X_q.col(k-1)) * B_rot);
     doublereal A_arr[3] = { 0, 0, 0 };
     doublereal *A_ptr = A_arr;
     
     // bl is an array of dimension at least n + nclin containing the lower bounds l in prob- lem LCQP.
     // To specify a non-existent bound (lj = −∞), set bl(j) ≤ −bigbnd,
     // where bigbnd is the Infinite Bound (default value 1020).
     // To specify an equality constraint rj(x) = β, set bl(j) = bu(j) = β, where |β| < bigbnd.
     double infinity = -100000000000;
     doublereal bl[4] = { -infinity, -infinity, -infinity, -infinity };
     doublereal *bl_ptr = bl;
     
     // bu is an array of dimension at least n+nclin containing the upper bounds u in problem LCQP.
     // To specify a non-existent bound (uj = ∞), set bu(j) ≥ bigbnd.
     // The bounds must satisfy bl(j) ≤ bu(j) for all j.
     // double lambda = 2.4;
     // mat u_b_ofA = -2 * lambda * E + 2 * X_dot.col(0).t() * M(X.col(0)) * (f(X.col(0)) + B * u_nom) - 2 * X_dot.col(k-1).t() * M(X.col(k-1)) * (f(X.col(k-1)) + B*u_nom);
     // double u_b_ofA_d = u_b_ofA(0);
     //double lambda = 1.0;
     //     mat u_b_ofA = -2 * lambda * E + 2 * Xq_dot.col(0).t() * (M(X_q.col(0)) * (f_rot(X_xi.col(0)) + B_rot * torque_nom))
     //                   - 2 * Xq_dot.col(k-1).t() * (M(X_q.col(k-1)) * (f_rot(X_xi.col(k-1)) + B_rot*torque_nom));
     
     double u_b_ofA_d = 0;
     
     doublereal bu[4] = { infinity, infinity, infinity, u_b_ofA_d };
     doublereal *bu_ptr = bu;
     //cout << "u_b_ofA" << u_b_ofA << endl;
     
     // cvec is an array of dimension at least n that contains the explicit linear term c of the objective.
     // If the problem is of type FP, QP1, or QP3, cvec is not referenced.
     // (In that case, cvec may be dimensioned (1), or it could be any convenient array.)
     //int m = 2;        // move this
     
     //mat cvec_mat = ((2 * eps_u)) * (torque_nom - u_prev);
     doublereal cvec[3] = { 0, 0, 0 };    // in NMPC_cont, cvec is [0; 0] --> maybe make into column?
     doublereal *cvec_ptr = cvec;
     
     //  is an array of dimension (ldH,k) for some k ≥ n.
     // H may be used to store the matrix H associated with the quadratic term of the QP objective.
     // It is not referenced if the problem type is FP or LP.
     // (In that case, H may be dimensioned (ldH,1) with ldH = 1,
     // or it could be any convenient array.)
     float eps_u = 2.5f;
     doublereal H[9] = { 1+eps_u, 0.0, 0.0,
         0, 1+eps_u, 0,
         0, 0, 1+eps_u };    // edited
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
     //     doublereal *clambda_ptr = clambda;
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
     
     qpopt_( &num_vars, &nclin, &ldA, &ldH, A_ptr, bl_ptr, bu_ptr, cvec_ptr, H_ptr, (U_fp)qphess_,
            &istate, x_ptr, &inform, &iter, &obj, Ax_ptr, clambda, iw_ptr, &leniw, w_ptr, &lenw );
//    AuxiliaryController *auxiliaryController = new AuxiliaryController();
//
//    // mat x_d; 
//    // x_d << 0                   << endr
//    //     << 0.000499999979166667 << endr
//    //     << 0.000999999833333342 << endr
//    //     << 0.00149999943750006;
//
//    // mat y_d;
//    // y_d << 0                    << endr
//    //     << 0.000249999997395833  << endr
//    //     << 0.000499999979166667 << endr
//    //     << 0.000749999929687502  << endr;
//
//    // mat v_d;
//    // v_d << 0.111803398874990 << endr
//    //     << 0.111803386995879 << endr
//    //     << 0.111803351358550 << endr
//    //     << 0.111803291963015;
//
//    // mat theta_d;
//    // theta_d << 0.523598775598299 << endr
//    //         << 0.523598813098302 << endr
//    //         << 0.523598925598347 << endr
//    //         << 0.523599113098543;
//
//    // mat a_d;
//    // a_d << 0                     << endr
//    //     << -4.64262676179249e-06 << endr
//    //     << -9.28525198080671e-06 << endr
//    //     << -1.39278741142640e-05;
//
//    // mat om_d;
//    // om_d << 0                    << endr
//    //      << 1.75194531295994e-05 << endr
//    //      << 3.50389214833707e-05 << endr
//    //      << 5.25584202855193e-05;
//
//    // mat x_act;
//    // x_act << -0.0103598477851339   <<  0.0187786546049586  << 0.111803398874990  <<  0.531472233397651 << endr
//    //       << -0.00987794054811401  <<  0.0190619661206226  << 0.109073584267891  << 0.519990704847360  << endr
//    //       << -0.00940465728719155  <<  0.0193329442093248  << 0.110872610057614  << 0.508384417446513  << endr
//    //       << -0.00892040339045780  <<  0.0196027896373440  << 0.112518374538164  << 0.496862712891326;
//
//    // mat u_prev;
//    // u_prev << 0     <<  -0.555962921419638      << 0.349805157944637 << 0.319152896109885 << endr
//    //        << 0     <<  -2.30630571005826       << -2.33125748016945 << -2.31434091103728;
//
//    mat state_nom;
//    state_nom.load("/Users/maggiewang/Workspace/QuadcoptersWorkspace/NMPC_ARM/src/state_nom.dat", raw_ascii);
//
//    mat ctr_nom;
//    ctr_nom.load("/Users/maggiewang/Workspace/QuadcoptersWorkspace/NMPC_ARM/src/ctrl_nom.dat", raw_ascii);
//
//    mat x_act;
//    x_act.load("/Users/maggiewang/Workspace/QuadcoptersWorkspace/NMPC_ARM/src/x_act.dat", raw_ascii);
//
//    mat ang_a;
//    ang_a.load("/Users/maggiewang/Workspace/QuadcoptersWorkspace/NMPC_ARM/src/ang_a.dat", raw_ascii);
//
//    mat accel;
//    accel.load("/Users/maggiewang/Workspace/QuadcoptersWorkspace/NMPC_ARM/src/accel.dat", raw_ascii);
//
//    //auxiliaryController->ComputeAuxiliaryController(state_nom.row(0).t(), ctr_nom.row(0).t(), x_act.row(0).t());
//    
//    // }
//
//    mat u_prev;
//    u_prev << 0 << 0 << 0; // << endr
//          // <<  0.0918267826181671 << -0.0106908896267330 << -0.00617717557794972;
//        // << 6.64802878082237e-16   << 0.0216554560694881 << 0 <<  0 << endr
//        // << 5.19413648470084e-10  <<  0.0216474901933048 << -0.000139674490490471   << -7.18189268538697e-05;
//    for (int i = 0; i < 4000; i++) {
//        auxiliaryController->ComputeAuxiliaryController(state_nom.row(i).t(), ctr_nom.row(i).t(), x_act.row(i).t(), ang_a.row(i).t(), accel.row(i).t(), i);
//    }
//
    return 0;
 }