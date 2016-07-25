% QPSUMMARY controls the summary file printing for qpopt.
%     qpsummary( filename )
%
%     qpsummary filename - causes qpopt to write an abbreviated summary
%                          of it's progress to filename.
%
%     qpsummary off      - tells qpopt not to write summary information
%
%     qpsummary on       - test qpopt to write summary information to
%                          "standard output". Under UNIX, "standard output"
%                          will be the terminal with your Matlab prompt.
%                          For other operating systems,  standard output is
%                          unlikely to be anything useful.
%
%     Summary printing is off by default.
function qpsummary( filename )

if strcmp( filename, 'on' )
    qpprm ( 'Summary file = 6' );
elseif strcmp( filename, 'off' )
    qpoptcmex( 6 );
else
    qpoptcmex( 5, filename );
end
