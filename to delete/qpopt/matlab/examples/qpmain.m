%     ------------------------------------------------------------------
%     Indefinite quadratic programming problem of Bunch and Kaufman,
%     `A computational method for the indefinite quadratic programming
%     problem', Linear Algebra and its Applications, 34, 341-370 (1980).
%
%     H      = the QP Hessian matrix.
%     A      = the general constraint matrix.
%     bl     = the lower bounds on  x  and  Ax.
%     bu     = the upper bounds on  x  and  Ax.
%     x      = the initial estimate of the solution.
%     ------------------------------------------------------------------

  c = [  7   6   5   4   3   2   1   0 ]';
  A = [ -1   1   0   0   0   0   0   0
         0  -1   1   0   0   0   0   0
         0   0  -1   1   0   0   0   0
         0   0   0  -1   1   0   0   0
         0   0   0   0  -1   1   0   0
         0   0   0   0   0  -1   1   0
         0   0   0   0   0   0  -1   1 ];
 bl = [ -1 -2.1 -3.2 -4.3 -5.4 -6.5 -7.6 -8.7 ...
        -1 -1.05 -1.1 -1.15 -1.2 -1.25 -1.3 ]';
 bu = [  1   2    3    4    5    6    7    8  inf inf inf inf inf inf inf ]';
  H = [  1.69  1     2     3     4     5     6     7
         1     1.69  1     2     3     4     5     6
         2     1     1.69  1     2     3     4     5
         3     2     1     1.69  1     2     3     4
         4     3     2     1     1.69  1     2     3
         5     4     3     2     1     1.69  1     2
         6     5     4     3     2     1     1.69  1
         7     6     5     4     3     2     1     1.69 ];
  x = [ -1    -2    -3    -4    -5    -6    -7    -8 ]';

  xopt1 = [ -1 -2 -3.05 -4.15 -5.3 6 7 8 ]';
  xopt2 = [ -1 -2.1 -3.15 -4.25 -5.4 6 7 8 ]';
  xopt3 = [ 1 2 1.880144 .780144 -.369856 -1.569856 -2.819856 -4.119856 ]';
 msglvl = 5;