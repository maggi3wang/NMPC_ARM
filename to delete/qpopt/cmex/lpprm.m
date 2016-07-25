% function lpprm ( option )
%     Sets an optional parameter of lpopt. The string "option" will be
%     read by lpopt.  If the string contains a setting that lpopt
%     understands,  lpopt will set internal parameters accordingly.
%     For a description of the parameters available,  see the lpopt
%     documentation.
%
%     MATLAB users should rarely need to call lpprm.  It should be
%     possible to use the mex (MATLAB) version of lpopt without ever
%     directly calling lpprm.
%
%     Do not try to set the unit number of the summary or print file.
%     Use the MATLAB functions npsummary and npprintfile instead.
function lpprm ( option )

lpoptcmex( 2, option );