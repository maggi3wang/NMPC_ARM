#include "AuxiliaryController.h"
#include <stdio.h>
#include <armadillo>
#include "f2c.h"

using namespace std;
using namespace arma;
using namespace f2c;

 int main() {
    AuxiliaryController *auxiliaryController = new AuxiliaryController();

    mat x_d; 
    x_d << 0                   << endr
        << 0.000499999979166667 << endr
        << 0.000999999833333342 << endr
        << 0.00149999943750006;

    mat y_d;
    y_d << 0                    << endr
        << 0.000249999997395833  << endr
        << 0.000499999979166667 << endr
        << 0.000749999929687502  << endr;

    mat v_d;
    v_d << 0.111803398874990 << endr
        << 0.111803386995879 << endr
        << 0.111803351358550 << endr
        << 0.111803291963015;

    mat theta_d;
    theta_d << 0.523598775598299 << endr
            << 0.523598813098302 << endr
            << 0.523598925598347 << endr
            << 0.523599113098543;

    mat a_d;
    a_d << 0                     << endr
        << -4.64262676179249e-06 << endr
        << -9.28525198080671e-06 << endr
        << -1.39278741142640e-05;

    mat om_d;
    om_d << 0                    << endr
         << 1.75194531295994e-05 << endr
         << 3.50389214833707e-05 << endr
         << 5.25584202855193e-05;

    mat x_act;
    x_act << -0.0103598477851339   <<  0.0187786546049586  << 0.111803398874990  <<  0.531472233397651 << endr
          << -0.00987794054811401  <<  0.0190619661206226  << 0.109073584267891  << 0.519990704847360  << endr
          << -0.00940465728719155  <<  0.0193329442093248  << 0.110872610057614  << 0.508384417446513  << endr
          << -0.00892040339045780  <<  0.0196027896373440  << 0.112518374538164  << 0.496862712891326;

    mat u_prev;
    u_prev << 0     <<  -0.555962921419638      << 0.349805157944637 << 0.319152896109885 << endr
           << 0     <<  -2.30630571005826       << -2.33125748016945 << -2.31434091103728;

    for (int i = 0; i < 4; i++) {
        auxiliaryController->ComputeAuxiliaryController(x_d(i), y_d(i), v_d(i), theta_d(i), a_d(i), om_d(i), x_act.row(i).t());
    }

    return 0;
 }