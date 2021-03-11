function results = sar_panel(y,x,W,T,info)
% PURPOSE: computes spatial lag model estimates for spatial panels (N regions*T time periods)
%           y = p*W*y + X*b + e, using sparse matrix algorithms
% Supply data sorted first by time and then by spatial units, so first region 1,
% region 2, et cetera, in the first year, then region 1, region 2, et
% cetera in the second year, and so on
% sem_panel computes y and x in deviation of the spatial and/or time means
% (see Baltagi, 2001, Econometric Analysis of Panel Data, ch. 2 and ch. 3)
% ---------------------------------------------------
%  USAGE: results = sar_panel(y,x,W,T,info)
%  where:  y = dependent variable vector
%          x = independent variables matrix
%          W = spatial weights matrix (standardized)
%          T = number of points in time
%       info = an (optional) structure variable with input options:
%       info.model = 0 pooled model without fixed effects (default, x may contain an intercept)
%                  = 1 spatial fixed effects (x may not contain an intercept)
%                  = 2 time period fixed effects (x may not contain an intercept)
%                  = 3 spatial and time period fixed effects (x may not contain an intercept)
%       info.rmin  = (optional) minimum value of rho to use in search  
%       info.rmax  = (optional) maximum value of rho to use in search    
%       info.convg = (optional) convergence criterion (default = 1e-8)
%       info.maxit = (optional) maximum # of iterations (default = 500)
%       info.lflag = 0 for full lndet computation (default = 1, fastest)
%                  = 1 for MC lndet approximation (fast for very large problems)
%                  = 2 for Spline lndet approximation (medium speed)
%       info.order = order to use with info.lflag = 1 option (default = 50)
%       info.iter  = iterations to use with info.lflag = 1 option (default = 30)  
%       info.lndet = a matrix returned by sar, sar_g, sarp_g, etc.
%                    containing log-determinant information to save time
% ---------------------------------------------------
%  RETURNS: a structure
%         results.meth  = 'psar' if infomodel=0
%                       = 'sarsfe' if info.model=1
%                       = 'sartfe' if info.model=2
%                       = 'sarstfe' if info.model=3
%         results.beta  = bhat
%         results.rho   = rho (p above)
%         results.tstat = asymp t-stat (last entry is rho=spatial autoregressive coefficient)
%         results.yhat  = yhat = [inv(y-p*W)]*x*b
%         results.resid = residuals = y-p*W*y-x*b
%         results.sige  = sige = (y-p*W*y-x*b)'*(y-p*W*y-x*b)/n
%         results.rsqr  = rsquared
%         results.rbar  = rbarsquared
%         results.sfe   = spatial fixed effects (if info.model=1 or 3)
%         results.tfe   = time period fixed effects (if info.model=2 or 3)
%         results.con   = intercept (if info.model=3)
%         results.lik   = log likelihood
%         results.nobs  = # of observations
%         results.nvar  = # of explanatory variables in x 
%         results.tnvar = nvar + W*y + # fixed effects
%         results.y     = y data vector
%         results.iter  = # of iterations taken
%         results.rmax  = 1/max eigenvalue of W (or rmax if input)
%         results.rmin  = 1/min eigenvalue of W (or rmin if input)
%         results.lflag = lflag from input
%         results.liter = info.iter option from input
%         results.order = info.order option from input
%         results.limit = matrix of [rho lower95,logdet approx, upper95] intervals
%                         for the case of lflag = 1
%         results.time1 = time for log determinant calcluation
%         results.time2 = time for eigenvalue calculation
%         results.time3 = time for hessian or information matrix calculation
%         results.time4 = time for optimization
%         results.time  = total time taken      
%         results.lndet = a matrix containing log-determinant information
%                          (for use in later function calls to save time)
% --------------------------------------------------
%  NOTES: if you use lflag = 1 or 2, info.rmin will be set = -1 
%                                    info.rmax will be set = 1
%         For number of spatial units < 500 you should use lflag = 0 to get exact results                                    
% ---------------------------------------------------
%
% written by: J.Paul Elhorst 11/2004
% University of Groningen
% Department of Economics
% 9700AV Groningen
% the Netherlands
% j.p.elhorst@eco.rug.nl
%
% REFERENCES: 
% "Specification and Estimation of Spatial Panel Data Models",
% International Regional Science Review, Vol. 26, pp. 244-268.
% Formulas for information matrix are not in this paper, I derived them
% later

% This function is based on James. P LeSage's function SAR

time1 = 0; 
time2 = 0;
time3 = 0;
time4 = 0;

timet = clock; % start the clock for overall timing

% if we have no options, invoke defaults
if nargin == 4
    info.lflag = 1;
    info.model=0;
    fprintf(1,'default: pooled model without fixed effects \n');
end;

fields = fieldnames(info);
nf = length(fields);
if nf > 0
    for i=1:nf
        if strcmp(fields{i},'model') model = info.model;
        end
    end
end
if model==0
    results.meth='psar';
elseif model==1
    results.meth='sarsfe';
elseif model==2
    results.meth='sartfe';
elseif model==3
    results.meth='sarstfe';
else
    error('sar_panel: wrong input number of info.model');
end

% check size of user inputs for comformability
[nobs nvar] = size(x);
[N Ncol] = size(W);
if N ~= Ncol
error('sar: wrong size weight matrix W');
elseif N ~= nobs/T
error('sar: wrong size weight matrix W or matrix x');
end;
[nchk junk] = size(y);
if nchk ~= nobs
error('sar: wrong size vector y or matrix x');
end;

% parse input options
[rmin,rmax,convg,maxit,detval,ldetflag,eflag,order,miter,options] = sar_parse(info); % function of LeSage

% compute eigenvalues or limits
[rmin,rmax,time2] = sar_eigs(eflag,W,rmin,rmax,N); % function of LeSage

% do log-det calculations
[detval,time1] = sar_lndet(ldetflag,W,rmin,rmax,detval,order,miter); % function of LeSage

for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    Wy([t1:t2],1)= sparse(W)*y([t1:t2],1);
end

% demeaning of the y and x variables, depending on (info.)model

if (model==1 | model==3);
meanny=zeros(N,1);
meannwy=zeros(N,1);
meannx=zeros(N,nvar);
for i=1:N
    ym=zeros(T,1);
    wym=zeros(T,1);
    xm=zeros(T,nvar);
    for t=1:T
        ym(t)=y(i+(t-1)*N,1);
        wym(t)=Wy(i+(t-1)*N,1);
        xm(t,:)=x(i+(t-1)*N,:);
    end
    meanny(i)=mean(ym);
    meannwy(i)=mean(wym);
    meannx(i,:)=mean(xm);
end
clear ym wym xm;
end % if statement

if ( model==2 | model==3)
meanty=zeros(T,1);
meantwy=zeros(T,1);
meantx=zeros(T,nvar);
for i=1:T
    t1=1+(i-1)*N;t2=i*N;
    ym=y([t1:t2],1);
    wym=Wy([t1:t2],1);
    xm=x([t1:t2],:);
    meanty(i)=mean(ym);
    meantwy(i)=mean(wym);
    meantx(i,:)=mean(xm);
end
clear ym wym xm;
end % if statement
    
en=ones(T,1);
et=ones(N,1);
ent=ones(nobs,1);

if model==1
    ywith=y-kron(en,meanny);
    wywith=Wy-kron(en,meannwy);
    xwith=x-kron(en,meannx);
elseif model==2
    ywith=y-kron(meanty,et);
    wywith=Wy-kron(meantwy,et);
    xwith=x-kron(meantx,et);
elseif model==3
    ywith=y-kron(en,meanny)-kron(meanty,et)+kron(ent,mean(y));
    wywith=Wy-kron(en,meannwy)-kron(meantwy,et)+kron(ent,mean(Wy));
    xwith=x-kron(en,meannx)-kron(meantx,et)+kron(ent,mean(x));
else
    ywith=y;
    wywith=Wy;
    xwith=x;
end % if statement

t0 = clock;
          AI = xwith'*xwith;
          b0 = AI\(xwith'*ywith);
          bd = AI\(xwith'*wywith);
          e0 = ywith - xwith*b0;
          ed = wywith - xwith*bd;
          epe0 = e0'*e0;
          eped = ed'*ed;
          epe0d = ed'*e0;

% step 1) do regressions
% step 2) maximize concentrated likelihood function;
	options = optimset('fminbnd');
    [p,liktmp,exitflag,output] = fminbnd('f_sarpanel',rmin,rmax,options,detval,epe0,eped,epe0d,N,T);
   
time4 = etime(clock,t0);

if exitflag == 0
fprintf(1,'sar: convergence not obtained in %4d iterations \n',output.iterations);
end;
results.iter = output.iterations;

% step 3) find b,sige maximum likelihood estimates
results.beta = b0 - p*bd; 
results.rho = p; 
bhat = results.beta;
results.sige = (1/nobs)*(e0-p*ed)'*(e0-p*ed); 
sige = results.sige;

if model==1
    results.sfe=meanny-meannwy*results.rho-meannx*results.beta; % including intercept
    xhat=x*results.beta+kron(en,results.sfe);
    tnvar=nvar+1+N; 
elseif model==2
    intercept=mean(y)-mean(Wy)*results.rho-mean(x)*results.beta; % intercept calculated separately
    results.con=intercept;
    results.tfe=meanty-meantwy*results.rho-meantx*results.beta-kron(en,intercept); 
    xhat=x*results.beta+kron(results.tfe,et)+kron(ent,intercept);
    tnvar=nvar+1+T;
elseif model==3
    intercept=mean(y)-mean(Wy)*results.rho-mean(x)*results.beta; % intercept calculated separately
    results.con=intercept;
    results.sfe=meanny-meannwy*results.rho-meannx*results.beta-kron(et,intercept);
    results.tfe=meanty-meantwy*results.rho-meantx*results.beta-kron(en,intercept);
    xhat=x*results.beta+kron(en,results.sfe)+kron(results.tfe,et)+kron(ent,intercept);
    tnvar=nvar+N+T;
else
    xhat=x*results.beta;
    tnvar=nvar+1; % +1 due to spatially lagged dependent variable
end    

yhat=zeros(nobs,1);
for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    yhat([t1:t2],1)=(speye(N) - p*sparse(W))\xhat([t1:t2],1);
end

results.yhat = yhat;
results.resid = y - p*Wy - xhat; 
results.xpxi=inv(xwith'*xwith); %new line
results.xwith=xwith; % new line

res=y-yhat; % new line
rsq=res'*res; % new line
results.rmse=sqrt(res); %new line

yme=y-mean(y);
rsqr2=yme'*yme;
rsqr1 = results.resid'*results.resid;
results.rsqr=1.0-rsqr1/rsqr2; %rsquared

results.r2=1.0-rsq/rsqr2; %r2 , alternative, new line

rsqr3 = rsqr1/(nobs-tnvar);
rsqr2 = rsqr2/(nobs-1.0);
results.rbar = 1 - (rsqr3/rsqr2); % rbar-squared
results.tnvar=tnvar;

parm = [results.beta
        results.rho
        results.sige];

results.lik = f2_sarpanel(parm,ywith,xwith,W,detval,T); %Elhorst

if N <= 500
t0 = clock;
% asymptotic t-stats based on information matrix (page 80-81 Anselin, 1980),
% adjusted by Elhorst for spatial panels 
B = speye(N) - p*sparse(W); 
BI = inv(B); WB = W*BI;
pterm = trace(WB*WB + WB*WB');
xpx = zeros(nvar+2,nvar+2);               
% bhat,bhat
xpx(1:nvar,1:nvar) = (1/sige)*(xwith'*xwith);     
% bhat,rho
ysum=zeros(nvar,1);
for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    ysum=ysum+(1/sige)*xwith([t1:t2],:)'*W*BI*xwith([t1:t2],:)*bhat;
end
xpx(1:nvar,nvar+1) = ysum;
xpx(nvar+1,1:nvar) = xpx(1:nvar,nvar+1)'; 
% rho,rho
ysom=0;
for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    ysom=ysom+(1/sige)*bhat'*xwith([t1:t2],:)'*BI'*W'*W*BI*xwith([t1:t2],:)*bhat + pterm;
end
xpx(nvar+1,nvar+1) = ysom;
% sige, sige
xpx(nvar+2,nvar+2) = nobs/(2*sige*sige);     
% rho,sige
xpx(nvar+1,nvar+2) = (T/sige)*trace(WB);  
xpx(nvar+2,nvar+1) = xpx(nvar+1,nvar+2);
xpxi = xpx\eye(size(xpx));
results.cov=xpxi(1:nvar+1,1:nvar+1); % new line
tmp = diag(xpxi(1:nvar+1,1:nvar+1));
bvec = [results.beta
        results.rho];
tmp = bvec./(sqrt(tmp));
results.tstat = tmp;
time3 = etime(clock,t0);

else  % asymptotic t-stats using numerical hessian
t0 = clock;
% just computes the diagonal
dhessn = hessian('f2_sarpanel',parm,ywith,xwith,W,detval,T); %Elhorst
hessi = invpd(dhessn);
tvar = abs(diag(hessi));
tmp = [results.beta
       results.rho];
results.tstat = tmp./sqrt(tvar(1:end-1,1));
time3 = etime(clock,t0);

end; % end of t-stat calculations

% return stuff
results.y = y;
results.nobs = nobs; 
results.nvar = nvar;
results.rmax = rmax;      
results.rmin = rmin;
results.lflag = ldetflag;
results.order = order;
results.miter = miter;
results.time = etime(clock,timet);
results.time1 = time1;
results.time2 = time2;
results.time3 = time3;
results.time4 = time4;
results.lndet = detval;