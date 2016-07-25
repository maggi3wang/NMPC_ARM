% LPSUMMARY controls the summary file printing for lpopt.
%     lpsummary( filename )
%
%     lpsummary filename - causes lpopt to write an abbreviated summary
%        of it's progress to filename.
%
%     lpsummary off - tells lpopt not to write summary information
%
%     lpsummary on - test lpopt to write summary information to
%         "standard output". Under UNIX, "standard output" will be the
%         terminal with your Matlab prompt. For other operating systems,
%         standard output is unlikely to be anything useful.
%
% Summary printing is off by default.
function lpsummary( filename )

if strcmp( filename, 'on' )
    lpprm ( 'Summary file = 6' );
elseif strcmp( filename, 'off' )
    lpoptcmex( 6 );
else
    lpoptcmex( 5, filename );
end
