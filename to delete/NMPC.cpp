#include "AuxiliaryController.h"
#include <stdio.h>
//#include <armadillo>

using namespace std;
//using namespace arma;

 int main() {
    AuxiliaryController *auxiliaryController = new AuxiliaryController();
    double geo_Ke = 10;
    // mat X;
    // mat X_dot;
    double J_opt = 0.001080589028186;
    int W;     // function handle
    int f;     // function handle
    // mat B;
    // mat u_nom;
    // mat lambda;
    auxiliaryController->ComputeAuxiliaryController();
    return 0;
 }

//#include "Geodesic.h"
//#include "AuxiliaryController.h"
//#include "libnpsol.h"
//#include "setup_geodesic_calc_nofunc.h"
//#include <stdio.h>
//#include <armadillo>
//
//// #include "matrix.h"
//// #include <mex.h>
//// #include <mat.h>
//#include <cstring>
//#include <chrono>
//
//using namespace std;
//using namespace arma;
//using namespace std::chrono;
//
//double fRand(double fMin, double fMax) {
//    double f = (double)rand() / RAND_MAX;
//    return fMin + f * (fMax - fMin);
//}
//
//int run_main(int argc, char **argv) {
//	if (!mclInitializeApplication(NULL,0)) {
//		cerr << "could not initialize the application properly" << endl;
//		return -1;
//	}
//
//	if(!libnpsolInitialize()) {
//		cerr << "could not initialize libnpsol properly" << endl;
//		return -1;
//	}
//
//   if (!setup_geodesic_calc_nofuncInitialize()) {
//      cerr << "could not initialize setup_geodesic_calc_nofunc properly" << endl;
//   }
//
//
//	try {
//		// Constants
//		int n = 4;
//		int m = 1;
//
//		// Setup Geodesic Numerics
//		float lambda = 0.25;
//		// find a way to load 'W_nmc.mat'
//		// find a way to do anonymous functions (get W and dW)
//		int geodesic_N = 4;
//
//    // for now
//    mat W_0;
//    W_0 << -0.175920612525947 << 0.410826105137175 << 0.159595074380887  << -0.564093471045887 << endr
//        << 0.410826105137175  << 1.54150365363617  << 0.455997256141696  << -1.67472165266172  << endr
//        << 0.159595074380887  << 0.455997256141696 << 3.71304119855472   << -0.979022195374093 << endr
//        << -0.564093471045887 << -1.67472165266172 << -0.979022195374093 << 2.47672565459105   << endr;
//
//    mat W_c;
//    W_c << 0.477548300596134  << -0.746939766616333  << -0.456746777463743   << 0.638568995453957    << endr
//        << -0.746939766616333 << 0.236729970621655   << -0.192032906275119   << -0.0291918466740831  << endr
//        << -0.456746777463743 << -0.192032906275119  << 0.0472138997678442   << -0.00932864268839913 << endr
//        << 0.638568995453957  << -0.0291918466740831 << -0.00932864268839913 << -0.258598150032931   << endr;
//
//		Geodesic *geodesic = new Geodesic();
//    high_resolution_clock::time_point t1 = high_resolution_clock::now();
//    geodesic->SetupGeodesicCalc(W_0, W_c);
//    high_resolution_clock::time_point t2 = high_resolution_clock::now();
//    auto duration = duration_cast<microseconds>( t2 - t1 ).count();
//    cout << "duration SetupGeodesicCalc: " << duration << endl;
//
//    // mat testState;
//    // testState <<  0.00 << endr
//    //           << -0.05 << endr
//    //           << -0.25 << endr
//    //           << -0.10 << endr;
//
//    // mat NMPC_state;
//    // NMPC_state << -0.0189665955946333 << -0.0220898346728266 << -0.110695505976391 << -0.140709371218691 << endr;
//
//    for (int i = 0; i < 10; i++) { // for now
//      // mat start_state;
//      // start_state << fRand(-datum::pi / 4.0, datum::pi / 4.0) << endr
//      //             << fRand(-0.6, 0.6) << endr
//      //             << fRand(-2.0, 2.0) << endr
//      //             << fRand(-1.0, 1.0) << endr;
//
//      // mat end_state;
//      // end_state << fRand(-datum::pi / 4.0, datum::pi / 4.0) << endr
//      //           << fRand(-0.6, 0.6) << endr
//      //           << fRand(-2.0, 2.0) << endr
//      //           << fRand(-1.0, 1.0) << endr;
//      mat testState;
//      testState <<  0.00 << endr
//                << -0.05 << endr
//                << -0.25 << endr
//                << -0.10 << endr;
//
//      mat NMPC_state;
//      NMPC_state << -0.0189665955946333 << -0.0220898346728266 << -0.110695505976391 << -0.140709371218691 << endr;
//
//      // cout << "start state:\n" << start_state << endl;
//      // cout << "end state:\n" << end_state << endl;
//      //geodesic->ComputeGeodesic(start_state, end_state);
//      geodesic->ComputeGeodesic(NMPC_state.t(), testState);
//    }   
//      // mxArray* geo_Prob;
//      // mxArray* K_e;
//      // mxArray* w_e;
//      // mxArray* T_e;
//      // mxArray* T_dot_e;
//      // mxArray* Aeq;
//      // mxArray* plhs[6] = { geo_Prob, K_e, w_e, T_e, T_dot_e, Aeq };
//      // mxArray* plhs[6];
//		// mxArray* plhs[6] = geodesic->SetupGeodesicCalc(W_0, W_c);
//
//  //     mat testState;
//  //     testState <<  0.00 << endr
//  //               << -0.05 << endr
//  //               << -0.25 << endr
//  //               << -0.10 << endr;
//
//  //     mat NMPC_state;
//  //     NMPC_state << -0.0189665955946333 << -0.0220898346728266 << -0.110695505976391 << -0.140709371218691 << endr;
//
//  //     mat hi = armaGetPr(plhs[0]);
//  //     //mat NMPC_state = { {-0.0189665955946333, -0.0220898346728266, -0.110695505976391, -0.140709371218691}};
//  //     geodesic->ComputeGeodesic(plhs[0], NMPC_state.t(), testState, plhs[3], plhs[4], plhs[5]);
//      //geodesic->ComputeGeodesic();
//
//      // // check q
//      // mat Q = kron(geo_we.t(),eye(m, m)); 
//      // mat Q_bar = Q.t() * Q;
//      // AuxiliaryController *auxiliaryController = new AuxiliaryController();
//      // // perhaps do a get geodesic vars or something
//      // Prob aux_Prob = auxiliaryController->SetupOptimalAuxiliary(geodesicVars geoVars, Q_bar, m);
//      // auxiliaryController->ComputeOptimalAuxiliary(geodesicVars geoVars);
//	//	mat geo_we = geodesic->GetGeoWe();
//	//
//	//	// Test Geodesic Numerics
//	//
//	//	mat Q = kron(geo_we.t(), eye(m, m));
//	//	mat Q_bar = Q.t() * Q;
//	//	AuxiliaryController* auxiliaryController = new AuxiliaryController();
//	//	auxProb = auxiliaryController->SetupOptimalAuxiliary();	// change inputs
//	//	auxiliaryController->ComputeOptimalAuxiliary(Q, lambda);
//
//	//compute_opt_aux(aux_Prob,Q,geo_we,geo_Ke,J_opt,X,X_dot,W,dW,...
//	//                f_aux,df_aux,B_aux,zeros(m,1),lambda,n,m);
//
//      //      // Create input data
//      //      double data[] = {1,2,3,4,5,6,7,8,9};
//      //      mwArray in1(3, 3, mxDOUBLE_CLASS, mxREAL);
//      //      mwArray in2(3, 3, mxDOUBLE_CLASS, mxREAL);
//      //      in1.SetData(data, 9);
//      //      in2.SetData(data, 9);
//      //
//      //      // Create output array
//      //      mwArray out;
//      //
//      //      // Call the library function
//      //      addmatrix(1, out, in1, in2);
//      //
//      //      std::cout << "The value of added matrix is:" << std::endl;
//      //      std::cout << out << std::endl;
//  }
//    catch (const mwException& e)
//  {
//    cerr << e.what() << endl;
//    return -2;
//  }
//    catch (...)
//  {
//    cerr << "Unexpected error thrown" << endl;
//    return -3;
//  }
//
//  libnpsolTerminate();
//  setup_geodesic_calc_nofuncTerminate();
//  mclTerminateApplication();
//  return 0;
//}
//
//int main() {
//  mclmcrInitialize();
//  cout << "finished initializing" << endl;
//  return mclRunMain((mclMainFcnType)run_main,0,NULL);
//}

