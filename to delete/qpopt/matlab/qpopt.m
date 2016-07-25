%QPOPT   General quadratic programming code QPOPT by Gill et. al.
%        [x,obj,lambda,istate,iter,inform] = qpopt(A,c,H,x,l,u,msglvl)
%        solves the problem
%                        minimize     c'x + 0.5*x'Hx
%                        subject to   ( )     ( x )    ( )
%                                     (l) =<  (   ) =< (u)
%                                     ( )     ( Ax)    ( )
%        On successful termination, x contains the optimal solution, and
%        obj contains the final objective function value. Lambda contains
%        the Lagrange multiplier vector, istate denotes the status of the
%        constraints and iter denotes the number of iterations. Inform
%        contains information about the exit status of QPOPT. Msglvl
%        determines the amount of output. Msglvl=0 gives no output,
%        msglvl = 1 gives one line per iteration.
%
%        This version modified for Matlab 4.0 on 15-Apr-1993.

%        M-file written by    Anders Forsgren
%                             Division of Optimization and Systems Theory
%                             Department of Mathematics
%                             Royal Institute of Technology
%                             S-100 44 Stockholm
%                             Sweden
%                             andersf@math.kth.se

function [x,obj,lambda,istate,iter,inform] = qpopt(A,cvec,H,x,bl,bu,msglvl)

if nargin < 7
   msglvl = 1;
end

[mA,nA] = size(A);
lcvec   = length(cvec);
[mH,nH] = size(H);
lx      = length(x);
lbl     = length(bl);
lbu     = length(bu);

if lcvec ~= lx  |  lbl ~= lbu  |  lbl ~= mA + lx  |  (nA > 0 & nA ~= lx) | mH ~= nH  |  nH ~= lx,
   clear mA nA lcvec mH nH lx lbl lbu obj lambda istate iter inform msglvl
   disp('                                                              ')
   disp(' --- Error using qpopt, incompatible dimensions of input data.')
   disp(' --- qpopt not run.  Check below and correct.                 ')
   disp('                                                              ')
   whos
else
   [x,obj,lambda,istate,iter,inform] = qpoptfmex(A,cvec,H,x,bl,bu,msglvl);
end
