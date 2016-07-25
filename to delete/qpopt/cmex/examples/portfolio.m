%portfolio.m  is a script that runs the portfolio investment problem
%  of Chapter 7, pp 258--262 of ``Numerical Linear Algebra and
%  Optimization'', by Gill, Murray and Wright, Addison Wesley, 1990).
%  At the Matlab prompt, type "portfolio".

%  ------------------------------------------------------------------
%  The portfolio investment problem of Chapter 7, pp 258--262 of
%  ``Numerical Linear Algebra and Optimization'', by Gill, Murray and
%  Wright, Addison Wesley, 1990).
%  The problem involves the rearrangement of a portfolio of three
%  stocks, Glitter, Risky and Trusty, so that the net worth of the
%  investor is maximized.
%  The problem is characterized by the following data:
%                             Glitter     Risky        Trusty
%  1990 Holdings                 75       1000           25
%  1990 Price/share($)           20          2          100
%  2099 Price/share($)           18          3          102
%  2099 Dividend                  5          0            2
%  ------------------------------------------------------------------
      clear A l u c x0

%     The variables x(1), x(2) and x(3) represent the change in each of
%     the three stocks.
%
%     Objective function:  maximize  5x(1) + 2x(3), or equivalently,
%     minimize -5x(1) - 2x(3).

      c = [  -5  0 -3 ]';

%     A nonnegative amount of stock must be present after rearrangement.
%     For Glitter:  x(1) +   75 ge 0.

      l(1) = -75;
      u(1) =  inf;

%     For Risky:    x(2) + 1000 ge 0.

      l(2) = -1000;
      u(2) =  inf;

%     For Trusty:   x(3) +   25 ge 0.

      l(3) = -25;
      u(3) =  inf;

%     The current value of the portfolio must be the same after
%     rearrangement, i.e.,
%     20(75+x(1)) + 2(1000+x(2)) + 100(25+x(3)) = 6000,   or
%     20x(1) + 2x(2) + 100x(3) = 0.

      A(1,1:3)  =  [ 20  2  100 ];
      l(4)      =     0;
      u(4)      =     0;

%     The value of the portfolio must increase by at least 5 per cent
%     at the end of the year, i.e.,
%     18(75+x(1)) + 3(1000+x(2)) + 102(25+x(3)) ge 6300,   or
%     18x(1) + 3x(2) + 102x(3) ge -600.

      A(2,1:3) =   [ 18  3  102 ];
      l(5)     = - 600;
      u(5)     =   inf;

%     ------------------------------------------------------------------
%     There are three ``balanced portfolio'' constraints.  The value of
%     a stock must constitute at least a quarter of the total final
%     value of the portfolio.  After rearrangement, the value of the
%     portfolio after is  20(75+x(1)) + 2(1000+x(2)) + 100(25+x(3)).
%     ------------------------------------------------------------------
%     If Glitter is to constitute at least a quarter of the final
%     portfolio, then   15x(1) - 0.5x(2) - 25x(3) ge 0.

      A(3,1:3) =   [ 15  -0.5  -25 ];
      l(6)     =     0;
      u(6)     =   inf;

%     If Risky is to constitute at least a quarter of the final
%     portfolio, then   -5x(1) + 1.5x(2) - 25x(3) ge -500.

      A(4,1:3) =   [ -5  1.5  -25 ];
      l(7)     =     -500;
      u(7)     =      inf;

%     If Trusty is to constitute at least a quarter of the final
%     portfolio, then   -5x(1) - 0.5x(2) + 75x(3) ge -1000.

      A(5,1:3) =   [ -5  -0.5  75 ];
      l(8)     =    -1000;
      u(8)     =      inf;

%     Set the initial estimate of the solution.
%     This portfolio is infeasible.

      x0       = [ 10  20  100 ]';

%     Make sure l and u are column vectors

      l = l';  u = u';

      format compact

      disp(' ')
      disp('Run 1:  Cold start with summary output:')

      lpsummary on
      [x,lambda,inform,iter,istate] = lpopt( A,l,u,c,x0 )
      lpsummary off

      disp(' ')
      disp('Run 2:  Warm start with no summary output:')
      [x,lambda,inform,iter] = lpopt( A,l,u,c,x0,istate )
