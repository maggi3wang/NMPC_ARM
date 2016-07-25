% QPPRINTFILE controls qpopt output to the print file.
%     qpprintfile( filename )
%     Causes qpopt to write detailed information about its progress in
%     solving the current problem to the file named in "filename."
%
%     "qpprintfile off" causes qpopt to stop writing to filename, and close
%     the file.
%
%     Note that until the file has been closed, it is unlikely to contain
%     all of the output.
function qpprintfile( filename )

if strcmp( filename, 'off' )
    qpoptcmex( 4 );
else
    qpoptcmex( 3, filename );
end