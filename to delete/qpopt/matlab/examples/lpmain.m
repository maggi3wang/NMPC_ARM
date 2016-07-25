%
%  Food               X     Y      Z   Fat   Enjoyment
%
%  Milk (quart)      50    10    150   800     200
%  Cookies (dozen)    3    10     35  6000    6000
%  Soup (cup)       150    75     75  1000    3000
%  Broccoli (pound) 100   100      5   400    -200

%  maximize ``enjoyment'' subject to the dietary restrictions that
%  at least: 600 units of vitamin X
%            300 units of vitamin Y
%            550 units of vitamin Z.
%
%     0  <=  Milk
%     0  <=  cookies
%     0  <=  Soup
%     0  <=  Broccoli
%
%

A      = [    50      3   150  100
              10     10    75  100
             150     35    75    5
             800   6000  1000  400 ];
c      = [  -200  -6000 -3000  200 ]';
bl     = [     0      0     0    0  600 300 550  -inf ]';
bu     = [   inf    inf   inf  inf  inf inf inf 13800 ]';
x      = [     0      0     0    0 ]';
msglvl = 5;