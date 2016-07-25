% function qpprm ( option )
%     Sets an optional parameter of qpopt. The string "option" will be
%     read by qpopt.  If the string contains a setting that qpopt
%     understands,  qpopt will set internal parameters accordingly.
%     For a description of the parameters available,  see the qpopt
%     documentation.
%
%     MATLAB users should rarely need to call qpprm.  It should be
%     possible to use the mex (MATLAB) version of qpopt without ever
%     directly calling qpprm.
%
%     Do not try to set the unit number of the summary or print file.
%     Use the MATLAB functions npsummary and npprintfile instead.
function qpprm ( option )

qpoptcmex( 2, option );