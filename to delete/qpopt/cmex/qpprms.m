% QPPRMS  reads optional parameters for qpopt.
%     [inform] = qpprms( filename )
%     Read the optional parameters from the named file.
%     The format of this file is described in the qpopt documentation.
%
%     Returns 0 if successful, and a positive number otherwise.
%
function inform = qpprms( filename )

inform = qpoptcmex( 1, filename );