% Script for testing lpopt.m

A  = [   50      3   150  100 
         10     10    75  100
         150    35    75    5 
         800  6000  1000  400 ];
c  = [  -200 -6000 -3000  200 ]';
l  = [     0     0     0    0  600 300 550  -inf ]';
u  = [   inf   inf   inf  inf  inf inf inf 13800 ]';
x0 = [     0     0     0    0 ]';

% evaluate all lpoptcmex variations with the wrong number of arguments
for i = 0:7
    fprintf( 'tag %d: Will fail with the wrong # of args\n', i );
    eval( ['lpoptcmex( ', int2str(i),',2, 3, 4, 5, 6, 7, 8 ,9, 10, 11)'],...
    'disp( lasterr )' );
end

fprintf( 'Will fail,  unrecognized function ID\n' );
eval( 'lpoptcmex( 8, 2, 3, 4, 5, 6, 7, 8 ,9, 10, 11)', 'disp(lasterr)' );

eval('lpmain');
disp('Correct answer: 0  0   13.8  0  ' );

% Copied from lpmain
A  = [   50      3   150  100 
         10     10    75  100
         150    35    75    5 
         800  6000  1000  400 ];

c  = [  -200 -6000 -3000  200 ]';

l  = [     0     0     0    0  600 300 550  -inf ]';

u  = [   inf   inf   inf  inf  inf inf inf 13800 ]';

x0 = [     0     0     0    0 ]';

fprintf( 'Should fail, not scalar\n' );
eval( 'lpopt( A, l, u, c, x0 )',...
    'disp(lasterr)' );

% Vectors must be column vectors
fprintf( 'Should fail, col != 1\n' );
eval(...
    'lpopt( A, l, u, [ 0 1 ], x0 )',...
    'disp(lasterr)' );

fprintf( 'Should fail, col != 1\n' );
eval(...
    'lpopt( A, l, [0 1], c, x0 )',...
    'disp(lasterr)' );

fprintf( 'Should fail, col != 1\n' );
eval(...
    'lpopt( A,  [0 1], u, c, x0 )',...
    'disp(lasterr)' );

% u and l must be the same size
fprintf( 'Should complain about the rows of u\n' )
eval(...
    'lpopt( A, [1;2], u, c, x0 )',...
    'disp(lasterr)' );

fprintf( 'Should complain about the cols of A\n' )
eval(...
    'lpopt( A'', l, u, c, x0 )',...
    'disp(lasterr)' );

fprintf( 'Should display a summary (under unix)\n' );
lpsummary on
lpmain;
fprintf( 'Should not display a summary\n' );
lpsummary off
lpmain;
fprintf( 'Should display a summary (under unix)\n' );
lpsummary on
lpmain;
fprintf( 'Should not display a summary\n' );
lpsummary off
lpmain;

unix( '\rm -f summaryFile' );
fprintf( 'Will now send a summary to a file, and then display it\n' )
lpsummary summaryFile
lpmain;
lpsummary off
type summaryFile

fprintf( 'Test to see if we are appending, should see two summaries\n' );
lpsummary summaryFile
lpmain;
lpsummary off
type summaryFile

unix( '\rm -f summaryFile' );
unix( '\rm -f summaryFile2' );

fprintf( 'Make sure we can switch summary files without calling "off"\n' )
lpsummary summaryFile
lpmain;
lpsummary summaryFile2
lpmain;
lpsummary off

fprintf( 'Should see one summary\n' )
type summaryFile
fprintf( 'and another summary\n' );
type summaryFile2

unix( '\rm -f summaryFile' );
unix( '\rm -f summaryFile2' );

unix( '\rm -f warmStart  echoOptions' );

fprintf( 'Does warm start work? Check file warmStart\n' );
istate0 = zeros(8,1);
lpprintfile  warmStart
lpopt( A, l, u, c, x0, istate0 );
lpprintfile  off

fprintf( 'Testing lpprms\n' );
fprintf(...
    'Check file echoOptions to make sure we read the file correctly\n' );
lpprintfile  echoOptions
lpprms       lpopt.opt;
lpprintfile  off
