%Test Script.

format compact;
setpath;  % defines the path

lpmain
[x,obj,lambda,istate,iter,inform] = lpopt(A,c,x,bl,bu,msglvl);

qpmain
[x,obj,lambda,istate,iter,inform] = qpopt(A,c,H,x,bl,bu,msglvl);