% LPPRMS  reads optional parameters for lpopt.
%     [inform] = lpprms( filename )
%     Read the optional parameters from the named file.
%     The format of this file is described in the lpopt documentation.
%
%     Returns 0 if successful, and a positive number otherwise.
%
function inform = lpprms( filename )

inform = lpoptcmex( 1, filename );