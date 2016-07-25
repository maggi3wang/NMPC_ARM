% QPOPT quadratic programming using an active set method.
%       [x,obj,lambda,inform,iter,istate] = qpopt( A,l,u,c,H,x0 )
%
%       Solves the problem
%                        minimize     c'x + (1/2)*x'Hx
%                        subject to           (  x )
%                                      l  =<  (    )  =<  u.
%                                             ( Ax )
%
% The parameter x0 is the starting point for the minimization problem.
%
% The output arguments are:
% x       - contains the minimizer.
% lambda  - the Lagrange multipliers at the solution.
% inform  - will be 0 if the iteration was sucessful.
% iter    - is the number of major iterations to find the solution.
% istate  -
%      a column vector. istate(j) !== 0 if and only if the j^{th} constraint
%      was equal to its upper or lower bound at the solution. If it was
%      at it's lower bound, istate(j) == 1. If it was at it's upper bound
%      istate(j) == 2. If istate(j) == 3, the constraint was actually an
%      equality constraint.
%
% Advanced Operation
%
% There is an additional optional input parameter.
%
%     function [x, lambda, inform, iter, istate] = ...
%        qpopt( A, l, u, c, x0, istate0 )
%
% istate0 -
%     Perform a "warm start" with istate0 as an initial guess as to which
%     constraints will be on their upper/lower bounds at the solution.
%     Can save time if there is some reason to believe that istate0 is
%     a good guess.

% Mike Gertz and Philip Gill:  15 Jan 2000

function [x,obj,lambda,inform,iter,istate] = qpopt(A,l,u,c,H,x0,istate0)

if ( nargin == 7 )
    [x,obj,lambda,inform,iter,istate] = qpoptcmex(0,A,l,u,c,H,x0,istate0);
else
    [x,obj,lambda,inform,iter,istate] = qpoptcmex(0,A,l,u,c,H,x0);
end
