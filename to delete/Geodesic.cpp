/*
 * Geodesic.cpp
 *
 *  Created on: Jun 21, 2016
 *      Author: maggiewang
 */

#include "Geodesic.h"
#include "libnpsol.h"
#include "armaMex.hpp"
#include <chrono>

using namespace std::chrono;

Geodesic::Geodesic() {
	n = 4;	// change
	N = 4; // change

	// check these numbers
	geo_Prob = (mxArray*)mxMalloc(100000 * sizeof(double));
	K_e = (mxArray*)mxMalloc(100 * sizeof(double));
	w_e = (mxArray*)mxMalloc(100 * sizeof(double));
	T_e = (mxArray*)mxMalloc(100 * sizeof(double));
	T_dot_e = (mxArray*)mxMalloc(100 * sizeof(double));
	Aeq = (mxArray*)mxMalloc(100 * sizeof(double));
// 	const char b_L_chars[] = "b_L";
// 	mexErrMsgTxt(b_L_chars);

// 	// geo_Prob
// 	const char *field_names[] = { "TOMLAB", "MATLAB", "Threads", "A", "ADObj", 
// 	"ADCons", "BIG", "b_L", "b_U", "c_L", 
// 	"c_U", "CheckNaN", "cName", "cols", "ConIx", 
// 	"ConsDiff", "ConsIdx", "ConsPattern", "d2cPattern", "d2LPattern", 
// 	"FAST", "fConstant","f_Low", "GATEF","GATEC", 
// 	"f_opt", "g_k", "GradTolg", "GradTolH", "GradTolJ",
//     "HessIx","HessPattern", "JacIx","JacPattern", "LargeScale", 
//     "MaxCPU","MENU", "Mode","nState", "N", 
//     "mLin", "mNonLin", "Name", "NumDiff", "P", 
//     "plotLine", "PriLev", "PriLevOpt", "probFile", "probType", 
//     "rows", "simType", "smallA", "SolverDLP", "SolverFP", 
//     "SolverLP", "SolverQP", "uP", "uPName", "WarmStart", "Warning", "x_0", "x_L", 
//     "x_U", "x_min", "x_max", "x_opt", "xName",
//     "QP", "LS", "MIP", "GO", "CGO", "ExpFit", 
//     "NTS", "LineParam", "optParam", "PartSep", 
//     "Solver", "FUNCS", "CONOPT", "DUNDEE", "GENO",
//     "glbDirect", "glcDirect","GP", "KNITRO", "LGO",
//     "LSGRG2", "MILPSOLVE", "MISQP", "OQNLP", "PENOPT",
//     "SOL", "CHECK" };

//     mwSize dims[2] = { 1, 2 };
//     int numOfFields = sizeof(field_names)/sizeof(*field_names);
//     geo_Prob = mxCreateStructArray(2, dims, numOfFields, field_names);
}

Geodesic::~Geodesic() {
	mxFree(geo_Prob);
	mxFree(K_e);
	mxFree(w_e);
	mxFree(T_e);
	mxFree(T_dot_e);
	mxFree(Aeq);
}

#define NUMBER_OF_STRUCTS (sizeof(friends)/sizeof(struct phonebook))
#define NUMBER_OF_FIELDS (sizeof(field_names)/sizeof(*field_names))

struct phonebook
{
  const char *name;
  double phone;
};

void Geodesic::SetupGeodesicCalc(mat W_0, mat W_c) {
	mxArray *W_0_in = armaCreateMxMatrix(W_0.n_rows, W_0.n_cols);
	armaSetData(W_0_in, W_0);

	mxArray *W_c_in = armaCreateMxMatrix(W_c.n_rows, W_c.n_cols);
	armaSetData(W_c_in, W_c);

	mxArray* prhs[] { mxCreateDoubleScalar(n), mxCreateDoubleScalar(N), W_0_in, W_c_in };
	mxArray* plhs[6] = { geo_Prob, K_e, w_e, T_e, T_dot_e, Aeq };

	high_resolution_clock::time_point t1 = high_resolution_clock::now();
	mlxSetup_geodesic_calc_nofunc(6, plhs, 4, prhs);
	high_resolution_clock::time_point t2 = high_resolution_clock::now();
	auto duration = duration_cast<microseconds>( t2 - t1 ).count();
	// cout << "duration mlxSetup_geodesic_calc_nofunc: " << duration << endl;
	// mxArray* geo_Prob2 = mxDuplicateArray(plhs[0]);
	geo_Prob = mxDuplicateArray(plhs[0]);
	K_e = mxDuplicateArray(plhs[1]);
	w_e = mxDuplicateArray(plhs[2]);
	T_e = mxDuplicateArray(plhs[3]);
	T_dot_e = mxDuplicateArray(plhs[4]);
	Aeq = mxDuplicateArray(plhs[5]);

	// geo_Prob
	const char *fieldNames[] = { "TOMLAB", "MATLAB", "Threads", "A", "ADObj", 
	"ADCons", "BIG", "b_L", "b_U", "c_L", 
	"c_U", "CheckNaN", "cName", "cols", "ConIx", 
	"ConsDiff", "ConsIdx", "ConsPattern", "d2cPattern", "d2LPattern", 
	"FAST", "fConstant","f_Low", "GATEF","GATEC", 
	"f_opt", "g_k", "GradTolg", "GradTolH", "GradTolJ",
    "HessIx","HessPattern", "JacIx","JacPattern", "LargeScale", 
    "MaxCPU","MENU", "Mode","nState", "N", 
    "mLin", "mNonLin", "Name", "NumDiff", "P", 
    "plotLine", "PriLev", "PriLevOpt", "probFile", "probType", 
    "rows", "simType", "smallA", "SolverDLP", "SolverFP", 
    "SolverLP", "SolverQP", "uP", "uPName", "WarmStart", "Warning", "x_0", "x_L", 
    "x_U", "x_min", "x_max", "x_opt", "xName",
    "QP", "LS", "MIP", "GO", "CGO", "ExpFit", 
    "NTS", "LineParam", "optParam", "PartSep", 
    "Solver", "FUNCS", "CONOPT", "DUNDEE", "GENO",
    "glbDirect", "glcDirect","GP", "KNITRO", "LGO",
    "LSGRG2", "MILPSOLVE", "MISQP", "OQNLP", "PENOPT",
    "SOL", "CHECK" };

// from mxcreatestructarray.c

	// const char *field_names[] = {"name", "phone"};
	// struct phonebook friends[] = {{"Jordan Robert", 3386},{"Mary Smith",3912},
	// 		  {"Stacy Flora", 3238},{"Harry Alpert",3077}};
	// mwSize dims[2] = { 1, NUMBER_OF_STRUCTS };
	// int name_field, phone_field;
	// mwIndex i;
    
 //    /* Create a 1-by-n array of structs. */ 
 //    cout << "NUMBER_OF_STRUCTS: " << NUMBER_OF_STRUCTS << endl;
 //    cout << "NUMBER_OF_FIELDS: " << NUMBER_OF_FIELDS << endl;
 //    cout << "field_names: " << field_names << endl;
 //    mxArray* myMxStruct = mxCreateStructArray(2, dims, NUMBER_OF_FIELDS, field_names);

 //    /* This is redundant, but here for illustration.  Since we just
 //       created the structure and the field number indices are zero
 //       based, name_field will always be 0 and phone_field will always
 //       be 1 */
 //    name_field = mxGetFieldNumber(myMxStruct,"name");
 //    phone_field = mxGetFieldNumber(myMxStruct,"phone");

 //    /* Populate the name and phone fields of the phonebook structure. */ 
 //    for (i=0; i<NUMBER_OF_STRUCTS; i++) {
 //        mxArray *field_value;
 //        /* Use mxSetFieldByNumber instead of mxSetField for efficiency
 //         * mxSetField(plhs[0],i,"name",mxCreateString(friends[i].name); */
 //        mxSetFieldByNumber(myMxStruct,i,name_field,mxCreateString(friends[i].name));
 //        field_value = mxCreateDoubleMatrix(1,1,mxREAL);
 //        *mxGetPr(field_value) = friends[i].phone;
 //        /* Use mxSetFieldByNumber instead of mxSetField for efficiency
 //         * mxSetField(plhs[0],i,"name",mxCreateString(friends[i].name); */
 //        mxSetFieldByNumber(myMxStruct,i,phone_field,field_value);
 //    }

 //    cout << "print telephone: " << endl;
 //    mxArray* mxarray_thing[] { mxCreateDoubleScalar(1) };
	// mxArray* input[] { myMxStruct };
	// mlxPrint(1, mxarray_thing, 1, input);
	// cout << "\n" << endl;

	/*
	NUMBER_OF_STRUCTS: 4
NUMBER_OF_FIELDS: 2
field_names: 0x11030d2b0
print telephone: 

thing = 

1x4 struct array with fields:

    name
    phone
*/

// PROBLEM WITH FUNCS!!!!!
    mxArray* mxArr = mxCreateDoubleMatrix( 0, 0, mxREAL );	// probably not right (?)
	const char FUNCS_chars[] = "FUNCS";
	mxSetField(geo_Prob, 0, FUNCS_chars, mxArr);

	// mxArray* mxarray_thing[] { mxCreateDoubleScalar(1) };
	// mxArray* input[] { geo_Prob };
	// mlxPrint(1, mxarray_thing, 1, input);
	// cout << "\n" << endl;

/*
    mwSize dims[1] = { 1 };	// check
    int numOfFields = sizeof(fieldNames)/sizeof(*fieldNames);
    cout << "num of Fields: " << numOfFields << endl;
    mxArray* geo_Prob_structArrayCopy = mxCreateStructArray(1, dims, numOfFields, fieldNames);
    
 //    const char tomlab_chars[] = "v8.2";
 //    mxArray* fieldValue = mxCreateString(tomlab_chars);
 //    mxSetFieldByNumber(geo_Prob_structArrayCopy, 0, 0, fieldValue);

 //    mxArray* fieldValue2 = mxDuplicateArray(mxGetField(geo_Prob, 0, mxGetFieldNameByNumber(geo_Prob, 2)));
 //    mxSetFieldByNumber(geo_Prob_structArrayCopy, 0, 2, fieldValue2);

 //    mxArray* fieldValue3 = mxDuplicateArray(mxGetField(geo_Prob, 0, mxGetFieldNameByNumber(geo_Prob, 93)));
 //    mxSetFieldByNumber(geo_Prob_structArrayCopy, 0, 93, fieldValue3);

    for (int i = 0; i < numOfFields; i++) {
    	// mxArray* fieldValue = mxDuplicateArray(mxGetField(geo_Prob, 0, mxGetFieldNameByNumber(geo_Prob, i)));
    	// mxSetFieldByNumber(geo_Prob_structArrayCopy, 0, i, fieldValue);
    	if (i != 79) {
    		mxArray* fieldValue = mxDuplicateArray(mxGetField(geo_Prob, 0, mxGetFieldNameByNumber(geo_Prob, i)));
    		mxSetFieldByNumber(geo_Prob_structArrayCopy, 0, i, fieldValue);
    	}
    	//mxSetFieldByNumber(geo_Prob_structArrayCopy, 0, valueField, mxCreateString(field_name[i].name));
    	// int classID = mxGetClassID(field_value);
    	// cout << "class ID: " << classID << endl;
  //   	cout << "field "<< i << ": " << mxGetFieldNameByNumber(geo_Prob, i) << endl;
		// mxArray* mxarray_thing[] { mxCreateDoubleScalar(1) };
		// mxArray* input[] { mxGetField(geo_Prob_structArrayCopy, 0, mxGetFieldNameByNumber(geo_Prob_structArrayCopy, i)) };
		// mlxPrint(1, mxarray_thing, 1, input);
	 //    cout << "\n" << endl;
    }
    //mxArray* fieldValue = mxGetField(geo_Prob, 0, mxGetFieldNameByNumber(geo_Prob, 2));
   
    mxArray* mxarray_thing[] { mxCreateDoubleScalar(1) };
	mxArray* input[] { geo_Prob_structArrayCopy };
	mlxPrint(1, mxarray_thing, 1, input);
	cout << "\n" << endl;

	*/

	// cout << "geo_Prob after mlx " << geo_Prob << endl;
	// cout << "geo_Prob class id: " << mxGetClassName(geo_Prob) << endl;
	// cout << "get number of fields: " << mxGetNumberOfFields(geo_Prob) << endl;
	// cout << "get number of elements: " << mxGetNumberOfElements(geo_Prob) << endl;
	// cout << "get number of dimensions: " << mxGetNumberOfDimensions(geo_Prob) << endl;
	// cout << "get dimensions: ";
	// const mwSize* dim = mxGetDimensions(geo_Prob);
	// for (int i = 0; i < mxGetNumberOfDimensions(geo_Prob); i++) {
	// 	cout << dim[i] << " ";
	// }
	// cout << "\n";
	
// geo_Prob class id: struct
// get number of fields: 95
// get number of elements: 1
// get number of dimensions: 2

	// cout << "geo_Prob class id: " << mxGetClassName(geo_Prob) << endl;
	// cout << "get number of fields: " << mxGetNumberOfFields(geo_Prob) << endl;
	// cout << "get number of elements: " << mxGetNumberOfElements(geo_Prob) << endl;
	// cout << "get number of dimensions: " << mxGetNumberOfDimensions(geo_Prob) << endl;
// 	geo_Prob class id: unknown
// get number of fields: 0
// get number of elements: 1
// get number of dimensions: 0

	// cout << "printing: " << endl;
	// mxArray* mxarray_thing[] { mxCreateDoubleScalar(1) };
	// mxArray* input[] { geo_Prob };
	// mlxPrint(1, mxarray_thing, 1, input);

	// K_e = plhs[1];
	// cout << "K_e class id: " << mxGetClassName(K_e) << endl;

	// cout << "get number of elements in setup: " << mxGetNumberOfElements(geo_Prob) << endl;

	// geo_Prob = plhs[0];
	// K_e = plhs[1];
	// w_e = plhs[2];
	// T_e = plhs[3];
	// T_dot_e = plhs[4];
	// Aeq = plhs[5];

	// // from mxcreatestructarray.c

	// const char *field_names[] = {"name", "phone"};
 //    struct phonebook friends[] = {{"Jordan Robert", 3386},{"Mary Smith",3912},
	// 			  {"Stacy Flora", 3238},{"Harry Alpert",3077}};
 //    mwSize dims[2] = { 1, NUMBER_OF_STRUCTS };
 //   int name_field, phone_field;
 //    mwIndex i;
    
    /* Create a 1-by-n array of structs. */ 
    // cout << "NUMBER_OF_STRUCTS: " << NUMBER_OF_STRUCTS << endl;
    // cout << "NUMBER_OF_FIELDS: " << NUMBER_OF_FIELDS << endl;
    // cout << "field_names: " << field_names << endl;
    //mxArray* myMxStruct = mxCreateStructArray(2, dims, NUMBER_OF_FIELDS, field_names);

 //    /* This is redundant, but here for illustration.  Since we just
 //       created the structure and the field number indices are zero
 //       based, name_field will always be 0 and phone_field will always
 //       be 1 */
 //    name_field = mxGetFieldNumber(myMxStruct,"name");
 //    phone_field = mxGetFieldNumber(myMxStruct,"phone");

 //    /* Populate the name and phone fields of the phonebook structure. */ 
 //    for (i=0; i<NUMBER_OF_STRUCTS; i++) {
 //        mxArray *field_value;
 //        /* Use mxSetFieldByNumber instead of mxSetField for efficiency
 //         * mxSetField(plhs[0],i,"name",mxCreateString(friends[i].name); */
 //        mxSetFieldByNumber(myMxStruct,i,name_field,mxCreateString(friends[i].name));
 //        field_value = mxCreateDoubleMatrix(1,1,mxREAL);
 //        *mxGetPr(field_value) = friends[i].phone;
 //        /* Use mxSetFieldByNumber instead of mxSetField for efficiency
 //         * mxSetField(plhs[0],i,"name",mxCreateString(friends[i].name); */
 //        mxSetFieldByNumber(myMxStruct,i,phone_field,field_value);
 //    }

	// for (int i = 0; i < sizeof(mxGetDimensions(geo_Prob)), i++) {
	// 	cout << "number of dimensions: " << mxGetDimensions(geo_Prob)[i] << endl;
	// }
	// mxArray *res = (mxArray*)mxMalloc(1000000);
	// // mxArray* mxarray_thing[] { mxCreateDoubleScalar(1) };
	// mxArray *result[1] = { res };
	//  mxArray *mx_geo_Prob[1] = { geo_Prob };
	// //mxArray *mx_struct[1] = { myMxStruct };
	
	// // cout << "before mlxProbcheck_npsol" << endl;;
	// mlxProbcheck_npsol(1, result, 1, mx_geo_Prob);
	// // //res = result[0];
	// // cout << "after mlxProbcheck_npsol" << endl;
	// cout << "mxGetNumberOfFields: " << mxGetNumberOfFields(plhs[0]) << endl;
}


// void Geodesic::SetupGeodesicCalc(int W, int dW) {
// 	int K = N + 4;
// 	int K_e = K + 2;

// 	// Obtain Chebyschev Pseudospectral Numerics
// 	// CGL points and quadrature weights
// 	pair<mat, mat> t_w = Clencurt(K);		// change variable names after understanding
// 	pair<mat, mat> t_e_w_e = Clencurt(K_e);

// 	w_e = t_e_w_e.second;

// 	pair<mat, mat> phi_start = ComputeCheby(0, -1);	// find out if second arg is supposed to be a mat
// 	pair<mat, mat> phi_end = ComputeCheby(0, 1);

// 	mat A_start = kron(eye<mat>(n, n), phi_start.first.t());
// 	mat A_end = kron(eye<mat>(n, n), phi_end.first.t());

// 	mat Aeq = join_vert(A_start, A_end);

// 	pair<mat, mat> T = ComputeCheby(K, t_w.first);
// 	pair<mat, mat> T_e = ComputeCheby(K_e, t_e_w_e.first);

// 	geoVars.T_e = T_e.first;
// 	geoVars.T_dot_e = T_e.second;

// 	Phi = zeros(n, n*(N+1), K+1);
// 	Phi_dot = zeros(n, n*(N+1), K+1);

// 	for (int i = 0; i <= K; i++) {
// 		Phi.slice(i) = kron(eye<mat>(n, n), T.first.col(i).t());
// 		Phi_dot.slice(i) = 2 * kron(eye<mat>(n, n), T.second.col(i).t());
// 	}
// 	// CHECKED UP TILL NOW!
// /*
// extern LIB_tomlab_cpp_lib_CPP_API void MW_CALL_CONV conAssign(int nargout, mwArray& Prob, 
// const mwArray& f, const mwArray& g, const mwArray& H, const mwArray& HessPattern, 
// const mwArray& x_L, const mwArray& x_U, const mwArray& Name, const mwArray& x_0, 
// const mwArray& pSepFunc, const mwArray& fLowBnd, const mwArray& A, const mwArray& b_L, 
// const mwArray& b_U, const mwArray& c, const mwArray& dc, const mwArray& d2c, const mwArray& ConsPattern, 
// const mwArray& c_L, const mwArray& c_U, const mwArray& x_min, const mwArray& x_max, const mwArray& f_opt, 
// const mwArray& x_opt);
// */
// 	// create output array
// 	// mwArray Prob;

//  	// f and g (anonymous functions) -- FIND OUT HOW TO DO THIS IN C++
//  	//function J = Geodesic_cost_tom(vars,w,n,K,W_fnc,Phi,Phi_dot)
// 	//mwArray f = GeodesicCostTom(vars, w, n, K, W_fnc, Phi, Phi_dot);
// 	//  g;		

// 	// all of the empty arrays:
// 	// mwArray H, HessPattern, x_L, x_U, pSepFunc, fLowBnd, c, dc, d2c, ConsPattern, c_L, c_U, x_min, x_max, f_opt, x_opt;	
//  // 	mwArray Name = "Geodesic Problem";
//  // 	mwArray x_0 = mwArray(2*n, 1, mxINT32_CLASS, mxREAL);	// creates 2*n by 1 matrix of zeros
// 	// //mwArray A = Aeq;		// FIGURE OUT HOW TO DO THIS
// 	// mwArray b_L = mwArray(2*n, 1, mxINT32_CLASS, mxREAL);
//  // 	mwArray b_U = mwArray(2*n, 1, mxINT32_CLASS, mxREAL);

// // // 23 inputs
// // // function Prob = conAssign(f, g, H, HessPattern, x_L, x_U, Name, x_0, ...
// // //                           pSepFunc, fLowBnd, ...
// // //                           A, b_L, b_U, c, dc, d2c, ConsPattern, c_L, c_U, ... 
// // //                           x_min, x_max, f_opt, x_opt)

// // // 19 inputs
// // //	Name = 'Geodesic Problem';
// // //	geo_Prob = conAssign(geo_cost_fnc,geo_grad_fnc,[],[],...
// // //	                  [],[],Name,zeros(n*(N+1),1),[],0,...
// // //	                  Aeq,zeros(2*n,1),zeros(2*n,1),[],[],[],[],[],[]);

// 	// 23 inputs
// 	// mlfConAssign(	1, Prob,										    // outputs
// 	// 				f, g,												// geo_cost_fnc, geo_grad_fnc
// 	// 				H, HessPattern,										// [], []
// 	// 				x_L, x_U,											// [], []
// 	// 				Name, x_0,											// Name, zeros(n*N+1, 1)
// 	// 				pSepFunc, fLowBnd,									// [], 0
// 	// 				A, b_L,												// Aeq, zeros(2*n, 1)
// 	// 				b_U, c,												// zeros(2*n, 1), []
// 	// 				dc, d2c,											// [], []
// 	// 				ConsPattern, c_L,									// [], []
// 	// 				c_U, x_min,											// [], []
// 	// 				x_max, f_opt,										// [], []
// 	// 				x_opt);												// [], []

// 	// input to struct
// 	//prob.A = Aeq;	// not sure
// 	// prob.name = 'Geodesic Problem';

// 	// // modifies starting point. if x_0 is outside bounds an error will be returned. 
// 	// // if idx is not given x_0 will be replaced
// 	// prob.x_0 = zeros(n*N+1, 1);
// 	// prob.b_L = zeros(2*n, 1);
// 	// prob.b_U = zeros(2*n, 1);

// 	// f and g (anonymous functions) -- FIND OUT HOW TO DO THIS IN C++
// 	//function J = Geodesic_cost_tom(vars,w,n,K,W_fnc,Phi,Phi_dot)
// 	//mwArray f = GeodesicCostTom(vars, w, n, K, W_fnc, Phi, Phi_dot);
// 	//  g;		
	

// 	// mat zeros1 = zeros(n*N+1, 1);	// change name soon
// 	// mat zeros2 = zeros(2*n, 1);		// change name soon

// /* FOR SETUP_GEODESIC_CALC_MEX.H
// 	cout << "Aeq:\n" << Aeq << endl;
	
// 	mxArray **plhs = new mxArray*[1];
// 	mxArray **prhs = new mxArray*[10];

// 	// w
// 	prhs[0] = armaCreateMxMatrix(t_w.second.n_rows, t_w.second.n_cols);
// 	armaSetData(prhs[0], t_w.second);

// 	// n
// 	prhs[1] = mxCreateDoubleScalar(n);	// check if this is right

// 	// K
// 	prhs[2] = mxCreateDoubleScalar(K);

// 	// W
// 	prhs[3] = mxCreateDoubleScalar(W);

// 	// Phi
// 	prhs[4] = armaCreateMxMatrix(Phi.n_rows, Phi.n_cols, Phi.n_slices);
// 	armaSetCubeData(prhs[4], Phi);

// 	// Phi_dot
// 	prhs[5] = armaCreateMxMatrix(Phi_dot.n_rows, Phi_dot.n_cols, Phi_dot.n_slices);
// 	armaSetCubeData(prhs[5], Phi_dot);

// 	// N
// 	prhs[6] = mxCreateDoubleScalar(N);

// 	// T
// 	prhs[7] = armaCreateMxMatrix(T.first.n_rows, T.second.n_cols);
// 	armaSetData(prhs[7], T.first);

// 	// dW
// 	prhs[8] = mxCreateDoubleScalar(dW);

// 	// Aeq   NOT SURE IF THIS WILL WORK!
// 	prhs[9] = armaCreateMxMatrix(Aeq.n_rows, Aeq.n_cols);
// 	armaSetData(prhs[9], Aeq);

// 	mlxSetup_geodesic_calc_mex(1, plhs, 10, prhs);

// 	*/

// 	// // possibly write a function / find a better way to do this
// 	// prhs[2] = mxCreateDoubleMatrix( 0, 0, mxREAL );
// 	// prhs[3] = mxCreateDoubleMatrix( 0, 0, mxREAL );
// 	// prhs[4] = mxCreateDoubleMatrix( 0, 0, mxREAL );
// 	// prhs[5] = mxCreateDoubleMatrix( 0, 0, mxREAL );
// 	// prhs[6] = 'Geodesic Problem';

// 	// prhs[7] = armaCreateMxMatrix(zeros1.n_rows, zeros1.n_cols);
// 	// armaSetData(prhs[7], zeros1);

// 	// prhs[8] = mxCreateDoubleMatrix( 0, 0, mxREAL );
// 	// prhs[9] = 0;
// 	// //plhs[10] = armaCreateMxMatrix()
// 	// prhs[11] = armaCreateMxMatrix(zeros2.n_rows, zeros2.n_cols);
// 	// armaSetData(prhs[11], zeros2);
// 	// prhs[12] = armaCreateMxMatrix(zeros2.n_rows, zeros2.n_cols);
// 	// armaSetData(prhs[12], zeros2);
// 	// prhs[13] = mxCreateDoubleMatrix( 0, 0, mxREAL );
// 	// prhs[14] = mxCreateDoubleMatrix( 0, 0, mxREAL );
// 	// prhs[15] = mxCreateDoubleMatrix( 0, 0, mxREAL );
// 	// prhs[16] = mxCreateDoubleMatrix( 0, 0, mxREAL );

// // 19 inputs
// //	Name = 'Geodesic Problem';
// //	geo_Prob = conAssign(geo_cost_fnc,geo_grad_fnc,[],[],...
// //	                  [],[],Name,zeros(n*(N+1),1),[],0,...
// //	                  Aeq,zeros(2*n,1),zeros(2*n,1),[],[],[],[],[],[]);

// // 23 inputs
// // function Prob = conAssign(f, g, H, HessPattern, x_L, x_U, Name, x_0, ...
// //                           pSepFunc, fLowBnd, ...
// //                           A, b_L, b_U, c, dc, d2c, ConsPattern, c_L, c_U, ... 
// //                           x_min, x_max, f_opt, x_opt)


// 	// mxArray* Name = "Geodesic Problem";
// 	// mxArray* x_0 = mxArray(2*n, 1, mxINT32_CLASS, mxREAL);	// creates 2*n by 1 matrix of zeros
// 	// //mwArray A = Aeq;		// FIGURE OUT HOW TO DO THIS
// 	// mxArray* b_L = mxArray(2*n, 1, mxINT32_CLASS, mxREAL);
// 	// mxArray* b_U = mxArray(2*n, 1, mxINT32_CLASS, mxREAL);

// 	// mxArray *mx_in1 = armaCreateMxMatrix(A.n_rows, A.n_cols);
// 	// armaSetData(mx_in1, A);

// 	// mxArray *mx_in2 = armaCreateMxMatrix(B.n_rows, B.n_cols);
// 	// armaSetData(mx_in2, B);

// 	// output
// 	// mxArray* plhs[] = { 1, Prob,										    // outputs
// 	// 					f, g,												// geo_cost_fnc, geo_grad_fnc
// 	// 					H, HessPattern,										// [], []
// 	// 					x_L, x_U,											// [], []
// 	// 					Name, x_0,											// Name, zeros(n*N+1, 1)
// 	// 					pSepFunc, fLowBnd,									// [], 0
// 	// 					A, b_L,												// Aeq, zeros(2*n, 1)
// 	// 					b_U, c,												// zeros(2*n, 1), []
// 	// 					dc, d2c,											// [], []
// 	// 					ConsPattern, c_L,									// [], []
// 	// 					c_U, x_min,											// [], []
// 	// 					x_max, f_opt,										// [], []
// 	// 					x_opt
// 	// 				  };

// 	//mlxConAssign(1, mxArray *plhs[], 19, mxArray *prhs[]);
// }


// TODO: make print not output anything
void Geodesic::ComputeGeodesic(mat start_p, mat end_p) {
	// mat vars_0 = zeros(n * (N+1), 1);
	// for (int i = 0; i < n; i++) {
	// 	vars_0((i-1) * (N+1) + linspace(1, i-1, i-1) * (N+1) + 2) =
	// 			join_horiz((start_p(i) + end_p(i)) / 2, -(start_p(i) - end_p(i)) / 2);
	// }
	// // replaces the linear constraints
	// prob.Aeq = A_eq;
	// prob.b_L = b_L;
	// prob.b_U = b_U;
	// prob.x_0 = vars_0;
	// prob = ProbCheck(prob, 'npsol');

	// Result = npsolTL(prob);

	// mat C_opt = reshape(Result.x_k, N+1, n).t();
	// mat X = C_opt * T_e;
	// mat X_dot = 2 * C_opt * T_dot_e;
	// // figure out how to do flags
	
	cout << "\n\n\n" << endl;
	mat beq = join_vert(start_p, end_p);

	mxArray *beq_mx = armaCreateMxMatrix(beq.n_rows, beq.n_cols);
	armaSetData(beq_mx, beq);
	//mat vars_0 = zeros(n*(N+1), 1);

	// for (int i = 0; i < n; i++) {
	// 	// vars_0((i-1)*(N+1)+1:(i-1)*(N+1)+2) = [(start_p(i)+end_p(i))/2; -(start_p(i)-end_p(i))/2];
	// 	//vars_0((i) * (N+1) + linspace(1, i-1, i-1) * (N+1) + 2) = join_horiz((start_p(i) + end_p(i)) / 2, -(start_p(i) - end_p(i)) / 2);
		
	// 	//cout << (i*(N+1) + linspace(1, (i*(N+1)), (i*(N+1))) + 2) << endl;
	// 	if 
	// }

	// just for now
	mat vars_0;
	vars_0 << -0.0095 << endr << 0.0095 << endr << 0 << endr << 0 << endr << 0 << endr << -0.0360 << endr 
	<< -0.0140 << endr << 0 << endr << 0 << endr << 0 << endr << -0.1803 << endr << -0.0697 << endr
	<< 0 << endr <<  0 << endr << 0 << endr << -0.1204 << endr << 0.0204 << endr << 0 << endr << 0 <<endr << 0 << endr;

	//const char tomlab_chars[] = "TOMLAB";
	//cout << "field: " << mxArrayToString(mxGetField(geo_Prob, 0, tomlab_chars)) << endl;
	
	const char b_L_chars[] = "b_L";
	const char b_U_chars[] = "b_U";
	// mxSetField(geo_Prob, 7, b_L_chars, beq_mx);
	// mxSetField(geo_Prob, 8, b_U_chars, beq_mx);
	mxSetField(geo_Prob, 0, b_L_chars, beq_mx);
	mxSetField(geo_Prob, 0, b_U_chars, beq_mx);

	const char x_0_chars[] = "x_0";
	mxArray *vars_0_mx = armaCreateMxMatrix(vars_0.n_rows, vars_0.n_cols);
	armaSetData(vars_0_mx, vars_0);
	//mxSetField(geo_Prob, 61, x_0_chars, vars_0_mx);
	mxSetField(geo_Prob, 0, x_0_chars, vars_0_mx);
	cout << "num of geo_Prob fields: " << mxGetNumberOfFields(geo_Prob) << endl;

	// // delete (is this necessary??)
	// mxDestroyArray(beq_mx);
	// mxDestroyArray(vars_0_mx);

	// const char numdiff_chars[] = "NumDiff";
	// //cout << "numdiff: " << mxGetField(geo_Prob_afterProbCheck[0], 43, numdiff_chars) << endl;
	// mxArray* numdiff = mxCreateDoubleScalar(0);
	// mxSetField(geo_Prob, 43, numdiff_chars, numdiff);

	// mxArray* prhs_Replace_A[4] = { geo_Prob, Aeq, beq_mx, beq_mx }; 
	// mxArray* new_geo_Prob = (mxArray*)mxMalloc(100000 * sizeof(double));
	// mxArray* geo_Prob_afterReplaceA[] = { new_geo_Prob }; 
	// mlxReplace_A(1, geo_Prob_afterReplaceA, 4, prhs_Replace_A);
	// new_geo_Prob = geo_Prob_afterReplaceA[0];
	// cout << "num of geo_Prob_afterReplaceA fields: " << mxGetNumberOfFields(new_geo_Prob) << endl;

	
	// mxArray *W_0_in = armaCreateMxMatrix(W_0.n_rows, W_0.n_cols);
	// armaSetData(W_0_in, W_0);

	// mxArray *W_c_in = armaCreateMxMatrix(W_c.n_rows, W_c.n_cols);
	// armaSetData(W_c_in, W_c);

	// mxArray* prhs[] { mxCreateDoubleScalar(n), mxCreateDoubleScalar(N), W_0_in, W_c_in };
	// mxArray* plhs[6] = { geo_Prob, K_e, w_e, T_e, T_dot_e, Aeq };

	// high_resolution_clock::time_point t1 = high_resolution_clock::now();
	// mlxSetup_geodesic_calc_nofunc(6, plhs, 4, prhs);
	// high_resolution_clock::time_point t2 = high_resolution_clock::now();
	// auto duration = duration_cast<microseconds>( t2 - t1 ).count();
	// cout << "duration mlxSetup_geodesic_calc_nofunc: " << duration << endl;

	// geo_Prob = plhs[0];
	// K_e = plhs[1];
	// w_e = plhs[2];
	// T_e = plhs[3];
	// T_dot_e = plhs[4];
	// Aeq = plhs[5];

	// maybe make faster by seeing if check == 1 
	// const char npsol_chars[] = "npsol";
	// mxArray *name = mxCreateString(npsol_chars);
	// mxArray *prhs_ProbCheck[] { geo_Prob, name };
	// mxArray* geo_Prob_afterCheck = (mxArray*)mxMalloc(100000 * sizeof(double));	// make more exact
	// mxArray* geo_Prob_afterProbCheck[1] = { geo_Prob_afterCheck };
	// //mxArray* geo_Prob_afterProbCheck[1] = { geo_Prob };	// see if can directly put in geo_Prob
	// mlxProbCheck(1, geo_Prob_afterProbCheck, 2, prhs_ProbCheck);
	// geo_Prob_afterCheck = geo_Prob_afterProbCheck[0];

	// // PrintGeodesicProblem();

	// geo_Prob_afterCheck = geo_Prob_afterProbCheck[0];
	// cout << "num of geo_Prob fields: " << mxGetNumberOfFields(geo_Prob) << endl;
	// cout << "num of geo_Prob_afterCheck fields: " << mxGetNumberOfFields(geo_Prob_afterCheck) << endl;

	// cout << "mlxPrint geo_Prob:\n" << endl;
	// for (int i = 0; i < mxGetNumberOfFields(geo_Prob_afterCheck); i++) {
	// 	cout << "field "<< i << ": " << mxGetFieldNameByNumber(geo_Prob_afterCheck, i) << endl;
	// 	mxArray* mxarray_thing[] { mxCreateDoubleScalar(1) };
	// 	mxArray* input[] { mxGetField(geo_Prob_afterCheck, 0, mxGetFieldNameByNumber(geo_Prob_afterCheck, i)) };
	// 	mlxPrint(1, mxarray_thing, 1, input);
	// 	cout << "\n" << endl;
	// }
	//PrintGeodesicProblem();

	// //cout << "after ProbCheck" << endl;

	// mxArray *res = (mxArray*)mxMalloc(1000000);
	// mxArray *result[1] = { res };
	// mxArray *prhs_NpSolTL[1] = { geo_Prob_afterCheck };
	
	// cout << "before mlxNpsolTL" << endl;
	// mlxNpsolTL(1, result, 1, prhs_NpSolTL);
	// cout << "after mlxNpsolTL" << endl;
	//PrintGeodesicProblem();

	mxArray *res = (mxArray*)mxMalloc(1000000);
	mxArray* mxarray_thing[] { mxCreateDoubleScalar(1) };
	mxArray *result[1] = { res };
	mxArray *mx_geo_Prob[1] = { geo_Prob };

	high_resolution_clock::time_point t1 = high_resolution_clock::now();

	cout << "before mlxProbcheck_npsol" << endl;;
	mlxProbcheck_npsol(1, result, 1, mx_geo_Prob);
	cout << "after mlxProbcheck_npsol" << endl;

	high_resolution_clock::time_point t2 = high_resolution_clock::now();
	auto duration = duration_cast<microseconds>( t2 - t1 ).count();
	cout << "duration ComputeGeodesic microseconds: " << duration << endl;

	// PRINT GEO_PROB
	//PrintGeodesicProblem();

	// PRINT RESULT
	res = result[0];
	cout << "-----RESULT----" << endl;
	cout << "num of fields of res: " << mxGetNumberOfFields(res) << endl;
	for (int i = 0; i < mxGetNumberOfFields(res); i++) {
		cout << "field "<< i << ": " << mxGetFieldNameByNumber(res, i) << endl;
		mxArray* mxarray_thing[] { mxCreateDoubleScalar(1) };
		mxArray* input[] { mxGetField(res, 0, mxGetFieldNameByNumber(res, i)) };
		mlxPrint(1, mxarray_thing, 1, input);
	    cout << "\n" << endl;
	}

	//cout << "num of res fields: " << mxGetNumberOfFields(res) << endl;
	// cout << "num of result fields: " << mxGetNumberOfFields(result[0]) << endl;

	// const char result_chars[] = "Name";
	//cout << "result name: " << mxArrayToString(mxGetField(res, 0, result_chars)) << endl;
	// PrintGeodesicProblem();
	/////////////
	// mxArray* prhs_Replace_A[4] = { geo_Prob, Aeq, beq_mx, beq_mx }; 
	// mxArray* geo_Prob_afterReplaceA[] = { }; 
	// mlxReplace_A(1, geo_Prob_afterReplaceA, 4, prhs_Replace_A);

	// mxArray *vars_0_mx = armaCreateMxMatrix(vars_0.n_rows, vars_0.n_cols);
	// armaSetData(vars_0_mx, vars_0);

	// mxArray* prhs_Modify_x_0[2] = { geo_Prob_afterReplaceA[0], vars_0_mx };
	// mxArray* geo_Prob_afterModifyX0[] = { };
	// mlxModify_x_0(1, geo_Prob_afterModifyX0, 2, prhs_Modify_x_0);

	// high_resolution_clock::time_point t2 = high_resolution_clock::now();
	// auto duration = duration_cast<microseconds>( t2 - t1 ).count();
	// cout << "duration ComputeGeodesic: " << duration << endl;

	// const char npsol_chars[] = "npsol";
	// mxArray *name = mxCreateString(npsol_chars);
	// mxArray *prhs_ProbCheck[2] = { geo_Prob_afterModifyX0[0], name };
	// mxArray* geo_Prob_afterProbCheck[] = { };

	// mlxProbCheck(1, geo_Prob_afterProbCheck, 2, prhs_ProbCheck);

	// mxArray *result[] = { };
	// mxArray *prhs_NpSolTL[1] = { geo_Prob_afterProbCheck[0] };
	// mlxNpsolTL(1, result, 1, prhs_NpSolTL);

	// mlxProbCheck(1, plhs, 2, prhs);

	// mlxNpsolTL(1, plhs, 1, prhs);
}

void Geodesic::PrintGeodesicProblem() {
	cout << "mlxPrint geo_Prob:\n" << endl;
	cout << "get number of fields: " << mxGetNumberOfFields(geo_Prob) << endl;
	// mxArray* mxarray_thing[] { mxCreateDoubleScalar(1) };
	// mxArray* input[] { geo_Prob };
	// mlxPrint(1, mxarray_thing, 1, input);

	// // cout << mxGetField(geo_Prob, 2, mxGetFieldNameByNumber(geo_Prob, 2)) << endl;

	// const char tomlab_chars[] = "TOMLAB";
	// cout << "field: " << mxArrayToString(mxGetField(geo_Prob, 2, tomlab_chars)) << endl;
	
	for (int i = 0; i < mxGetNumberOfFields(geo_Prob); i++) {
	// 	//cout << "field name: " << mxGetFieldNameByNumber(geo_Prob, i) << endl;
	// 	mxArray 
	// 	mxArray* in = mxGetField(geo_Prob, 0, mxGetFieldNameByNumber(geo_Prob, i));
	// 	mxArray* input2[] { in };
	// 	mxArray* mxarr[] { mxCreateDoubleScalar(1) };
	// 	mlxPrint(1, mxarr, 1, input2);
	
		cout << "field "<< i << ": " << mxGetFieldNameByNumber(geo_Prob, i) << endl;
		mxArray* mxarray_thing[] { mxCreateDoubleScalar(1) };
		mxArray* input[] { mxGetField(geo_Prob, 0, mxGetFieldNameByNumber(geo_Prob, i)) };
		mlxPrint(1, mxarray_thing, 1, input);
	    cout << "\n" << endl;
		//cout << "mxarray_thing[0]:\n" << mxGetNumberOfFields(mxarray_thing[0]) << endl;
	}
}

// pair<mat, mat> Geodesic::Clencurt(int K) {
// 	double k = double(K);
// 	rowvec theta = M_PI * linspace<rowvec>(0, k, k+1) / k;
// 	rowvec x = fliplr(cos(theta));

// 	mat w_net;

// 	if (K % 2 == 0) {		// maybe do odd case too (?)
// 		rowvec w = zeros<rowvec>(k/2+1);	// zeros 1 x (K/2+1)
// 		w(0) = 1/(k*k - 1);
// 		rowvec w_pre = 0.5 + 0.5 * 1/(1-k*k) * cos(M_PI*(linspace<rowvec>(1, K/2, K/2)));
// 		vec j = linspace(1, k/2-1, k/2-1);

// 		for (int i = 0; i < k/2; i++) {
// 			w(i+1) = (4/k) * w_pre(i) + (4/k) * sum((1/(1- 4 * (j % j))) % cos(2*M_PI*j*(i+1)/k));
// 		}
// 		w.shed_col(w.n_cols - 1);
// 		w_net = join_horiz(w, fliplr(w)).t();
// 	}
// 	return make_pair(x, w_net);
// }

// // make this method compatible with mat t and int t
// pair<mat, mat> Geodesic::ComputeCheby(int K, mat t) {
// 	mat T = zeros(N+1, K+1);
// 	mat U = zeros(N+1, K+1);
// 	mat T_dot = zeros(N+1, K+1);

// 	T.row(0) = rowvec(K+1).fill(1.0);	// see if can do this w only constructor
// 	T.row(1) = t;

// 	U.row(0) = rowvec(K+1).fill(1.0);
// 	U.row(1) = 2 * t;

// 	T_dot.row(1) = rowvec(K+1).fill(1.0);

// 	for (int i = 1; i < N; i++) {
// 		T.row(i+1) = 2.0 * (t % T.row(i)) - T.row(i-1);
// 		U.row(i+1) = 2.0 * (t % U.row(i)) - U.row(i-1);
// 		T_dot.row(i+1) = (i+1) * U.row(i);
// 	}

// 	cout << "T:\n" << T << endl;
// 	cout << "U:\n" << U << endl;
// 	cout << "T_dot:\n" << T_dot << endl;

// 	return make_pair(T, T_dot);
// }

// pair<mat, mat> Geodesic::ComputeCheby(int K, int t) {
// 	mat T = zeros(N+1, K+1);
// 	mat U = zeros(N+1, K+1);
// 	mat T_dot = zeros(N+1, K+1);

// 	T.row(0) = rowvec(K+1).fill(1.0);	// see if can do this w only constructor
// 	T.row(1) = t;

// 	U.row(0) = rowvec(K+1).fill(1.0);
// 	U.row(1) = 2 * t;

// 	T_dot.row(1) = rowvec(K+1).fill(1.0);

// 	for (int i = 1; i < N; i++) {
// 		T.row(i+1) = 2.0 * t * T.row(i) - T.row(i-1);
// 		U.row(i+1) = 2.0 * t * U.row(i) - U.row(i-1);
// 		T_dot.row(i+1) = (i+1) * U.row(i);
// 	}

// 	return make_pair(T, T_dot);
// }

// not sure if vars is mat or an argument -- check
// CLBL!
// mat Geodesic::GeodesicCostTom(mat vars, int w, int K, int W_fnc) {
// 	mat J = 0;
// 	for (int i = 0; i < K; i++) {
// 		mat x_k = Phi.slice(i) * vars;
// 		vec x_dot_k = Phi_dot.slice(i) * vars;
// //		mat W = W_fnc(x_k);
// //		mat M = solve(W, eye(n, n));
// //		mat M_xdot = M % x_dot_k;	// % or * (?)
// //		J = J + 0.5 * w(i) * (x_dot_k.t() * M_xdot);
// 	}
// 	return J;
// }

// // CLBL!
// mat Geodesic::GeodesicGrad(mat vars, int w, int K, mat T, int W_fnc, int dW_fnc) {
// 	mat GradObj = zeros(n*(N+1), 1);
// 	mat I = eye(n, n);

// 	for (int i = 0; i < K; i++) {
// 		mat x_k = Phi.slice(i) * vars;
// 		mat x_dot_k = Phi_dot.slice(i) * vars;

// 		mat W = W_fnc(x_k);
// 		mat M = solve(W, eye(n, n));
// 		mat M_xdot = M * x_dot_k;	// perhaps %
// 		mat W_dx = dW_fnc(x_k);

// 		GradObj = GradObj + w(i) * Phi_dot.slice(i).t() * M_xdot;

// 		for (int j = 0; j < n; j++) {
// 			GradObj = GradObj - 0.5 * w(i) * M_xdot.t() * W_dx(j) * M_xdot * kron(I.col(j), T.col(i));
// 		}
// 	}
// 	return GradObj;
// }

// mat Geodesic::GetGeoWe() {
// 	return w_e;
// }

// // replaces the linear constraints
// void Geodesic::ReplaceA(Prob prob, pair<mat, mat> Aeq, mat b_L, mat b_U) {
// 	prob.Aeq = A_eq;
// 	prob.b_L = b_L;
// 	prob.b_U = b_U;
// }

// // modifies starting point
// // if x_0 is outside bounds an error will be returned
// // if idx is not given x_0 will be replaced
// Prob Geodesic::ModifyX0(Prob prob, mat vars_0) {
// 	prob.x_0 = vars_0;
// }

// // go do some checks
// Prob Geodesic::ProbCheckForNPSolver(Prob prob) {

// }


// void Goedesic::npsol() {
// 	// long int --> integer
// 	// double --> doublereal
// 	// typedef int /* Unknown procedure type */ (*U_fp)(...);  <----- ??
// 	npsol_( &n, &nclin, &ncnln, &nrowa, &nrowj, &n /* nrowr */,
// 	A, l, u, confun_, objfun_, &inform, &iter, istate,
// 	c, cjac, lambda, &f, g, R, x, iw, &leniw, w, &lenw );
// }


// // extern int npsol_
// //   ( integer *n, integer *nclin, integer *ncnln, 
// //     integer *lda, integer *ldju, integer *ldr, 
// //     doublereal *a, doublereal *bl, doublereal *bu,
// //     U_fp funcon, U_fp funobj,
// //     integer *inform__, integer *iter, integer *istate,
// //     doublereal *c__, doublereal *cjacu,
// //     doublereal *clamda, doublereal *objf, doublereal *gradu,
// //     doublereal *r__, doublereal *x,
// //     integer *iw, integer *leniw, doublereal *w, integer *lenw );
