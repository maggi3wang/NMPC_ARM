%[x,obj,lambda,istate,iter,inform] = lpopt(A,c,x,bl,bu,msglvl);
%
%    Solves the dense linear programming problem:
%                    minimize            c'x
%                    subject to  bl <=  (  x )  <= bu
%                                       ( Ax )
%    On input:
%       msglvl  defines the amount of output
%               msglvl = 1 (default) prints one line per iteration
%               msglvl = 0 gives no output.
%
%
%    On successful termination:
%       x       contains the optimal solution
%       obj     contains the final objective function value
%       lambda  contains the Lagrange multipliers
%       istate  gives the status of the constraints
%       iter    gives the number of iterations
%       inform  contains information about the exit status of lpoptf
%
%    This version for Matlab 2007a  dated 19 Dec 2008.

%    First version: 14 Aug 1998: Anders Forsgren
%    f90   version: 19 Dec 2008: Philip Gill and Michael Saunders


function [x,obj,lambda,istate,iter,inform] = lpopt(A,cvec,x,bl,bu,msglvl)

if nargin < 6,
  msglvl = 1;
end

[mA,nA] = size(A);
lcvec   = length(cvec);
lx      = length(x);
lbl     = length(bl);
lbu     = length(bu);

if lcvec ~= lx  |  lbl ~= lbu  |  lbl ~= mA + lx  |  (nA > 0 & nA ~= lx),
   clear mA nA lcvec lx lbl lbu obj lambda istate iter inform msglvl
   disp('                                                              ')
   disp(' --- Error using lpopt, incompatible dimensions of input data.')
   disp(' --- lpopt not run.  Check below and correct.                 ')
   disp('                                                              ')
   whos
else
   [x,obj,lambda,istate,iter,inform] = lpoptfmex(full(A),cvec,x,bl,bu,msglvl);
end
