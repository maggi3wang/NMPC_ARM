% LPPRINTFILE controls lpopt output to the print file.
%     lpprintfile( filename )
%     Causes lpopt to write detailed information about its progress in
%     solving the current problem to the file named in "filename."
%
%     "lpprintfile off" causes lpopt to stop writing to filename, and close
%     the file.
%
%     Note that until the file has been closed, it is unlikely to contain
%     all of the output.
function lpprintfile( filename )

if strcmp( filename, 'off' )
    lpoptcmex( 4 );
else
    lpoptcmex( 3, filename );
end