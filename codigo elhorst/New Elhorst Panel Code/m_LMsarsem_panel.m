clear all;

A1=xlsread('F:\minmin\maize\玉米单产.xls',1, 'B2:AE28');
A2=xlsread('F:\minmin\maize\rd.xlsx',1, 'B2:AE28');
A3=xlsread('F:\minmin\maize\农业固定资产总投资.xls',1, 'B2:AE28');
A4=xlsread('F:\minmin\maize\农村劳动力文化状况.xls', 1,'B2:AE28');
A5=xlsread('F:\minmin\maize\降水.xlsx',1, 'B2:AE28');
A6=xlsread('F:\minmin\maize\温度.xlsx',1, 'B2:AE28');
A7=xlsread('F:\minmin\maize\水库总库容.xls',1, 'B2:AE28');
A8=xlsread('F:\minmin\maize\化肥费.xls',1, 'B2:AE28');
A9=xlsread('F:\minmin\maize\机械作业费.xls',1, 'B2:AE28');
A10=xlsread('F:\minmin\maize\乡村从业人员.xls',1, 'B2:AE28');
A11=xlsread('F:\minmin\maize\种子费.xls',1, 'B2:AE28');
A12=xlsread('F:\minmin\maize\玉米面积.xls',1, 'B2:AE28');
A13=xlsread('F:\minmin\maize\各地区受灾面积.xlsx',1, 'B2:AE28');
A14=xlsread('F:\minmin\maize\各地有效灌溉面积.xlsx',1, 'B2:AE28');
A15=xlsread('F:\minmin\maize\各地主要农作物播种面积.xlsx',1, 'B2:AE28');
A16=xlsread('F:\minmin\maize\农林牧渔业从业人员.xlsx',1, 'B2:AE28');
load('sp3');
% Dataset downloaded from www.wiley.co.uk/baltagi/
% Spatial weights matrix constructed by Elhorst
%
% written by: J.Paul Elhorst summer 2008
% University of Groningen
% Department of Economics
% 9700AV Groningen
% the Netherlands
% j.p.elhorst@rug.nl
%
% REFERENCE: 
% Elhorst JP (2009) Spatial Panel Data Models. In Fischer MM, Getis A (Eds.) 
% Handbook of Applied Spatial Analysis, Ch. C.2. Springer: Berlin Heidelberg New York.
%
% dimensions of the problem
T=30; % number of time periods
N=27; % number of regions
% row-normalize W
%W3=matrix(A1,N,T)*W1;
W=normw(W2); % function of LeSage
yield=reshape(A1,810,1); % column number in the data matrix that corresponds to the dependent variable
prec=reshape(A5,810,1);% column numbers in the data matrix that correspond to the independent variables
temp=reshape(A6,810,1);
rd=reshape(A2,810,1);
edu=reshape(A4,810,1);
fert=reshape(A8,810,1);
mach=reshape(A9,810,1);
labo=reshape(A16,810,1);
seed=reshape(A11,810,1);
area=reshape(A12,810,1)./reshape(A15,810,1);
dama=reshape(A13,810,1);
irri=reshape(A14,810,1);
inve=reshape(A3,810,1);
water=reshape(A7,810,1);
y=yield;
ymax=max(yield);
ymin=min(yield);
x=[prec temp edu fert mach labo seed area dama irri];
xmax=max(x);
xmin=min(x);
[y11,mu11,sigma11]=zscore(yield);
[x21,mu21,sigma21]=zscore(x);
y=log(y);
x=log(x+1);
[y,mu1,sigma1]=zscore(y);
[x,mu2,sigma2]=zscore(x);
xconstant=ones(N*T,1);
[nobs K]=size(x);
% ----------------------------------------------------------------------------------------
% ols estimation 
results=ols(y,[xconstant x]);
vnames=strvcat('yield','intercept','prec','temp','edu','fert','mach','labo','seed','area','dama','irri');
prt_reg(results,vnames,1);
sige=results.sige*((nobs-K)/nobs);
loglikols=-nobs/2*log(2*pi*sige)-1/(2*sige)*results.resid'*results.resid

% The (robust)LM tests developed by Elhorst

LMsarsem_panel(results,W,y,[xconstant x]); % (Robust) LM tests

% The lm tests developed by Donald Lacombe
% see http://www.rri.wvu.edu/lacombe/~lacombe.htm

lm1=lmlag_panel(y,[xconstant x],W);
prt_tests(lm1);

lm2=lmerror_panel(y,[xconstant x],W);
prt_tests(lm2);

lm3=lmlag_robust_panel(y,[xconstant x],W);
prt_tests(lm3);

lm4=lmerror_robust_panel(y,[xconstant x],W);
prt_tests(lm4);

% ----------------------------------------------------------------------------------------
% spatial fixed effects + (robust) LM tests for spatial lag and spatial error model
% fixed effects, within estimator
% demeaning of the y and x variables
model=1;
[ywith,xwith,meanny,meannx,meanty,meantx]=demean(y,x,N,T,model);
results=ols(ywith,xwith);
vnames=strvcat('yield','prec','temp','edu','fert','mach','labo','seed','area','dama','irri');
prt_reg(results,vnames);
sfe=meanny-meannx*results.beta; % including the constant term
yme = y - mean(y);
et=ones(T,1);
error=y-kron(et,sfe)-x*results.beta;
rsqr1 = error'*error;
rsqr2 = yme'*yme;
FE_rsqr2 = 1.0 - rsqr1/rsqr2 % r-squared including fixed effects
sige=results.sige*((nobs-K)/nobs);
logliksfe=-nobs/2*log(2*pi*sige)-1/(2*sige)*results.resid'*results.resid
LMsarsem_panel(results,W,ywith,xwith); % (Robust) LM tests

lm1=lmlag_panel(ywith,xwith,W);
prt_tests(lm1);

lm2=lmerror_panel(ywith,xwith,W);
prt_tests(lm2);

lm3=lmlag_robust_panel(ywith,xwith,W);
prt_tests(lm3);

lm4=lmerror_robust_panel(ywith,xwith,W);
prt_tests(lm4);
% ----------------------------------------------------------------------------------------
% time-period fixed effects + (robust) LM tests for spatial lag and spatial error model
% fixed effects, within estimator
% demeaning of the y and x variables
model=2;
[ywith,xwith,meanny,meannx,meanty,meantx]=demean(y,x,N,T,model);
results=ols(ywith,xwith);
vnames=strvcat('yield','prec','temp','edu','fert','mach','labo','seed','area','dama','irri');
prt_reg(results,vnames);
tfe=meanty-meantx*results.beta; % including the constant term
yme = y - mean(y);
en=ones(N,1);
error=y-kron(tfe,en)-x*results.beta;
rsqr1 = error'*error;
rsqr2 = yme'*yme;
FE_rsqr2 = 1.0 - rsqr1/rsqr2 % r-squared including fixed effects
sige=results.sige*((nobs-K)/nobs);
logliktfe=-nobs/2*log(2*pi*sige)-1/(2*sige)*results.resid'*results.resid
LMsarsem_panel(results,W,ywith,xwith); % (Robust) LM tests

lm1=lmlag_panel(ywith,xwith,W);
prt_tests(lm1);

lm2=lmerror_panel(ywith,xwith,W);
prt_tests(lm2);

lm3=lmlag_robust_panel(ywith,xwith,W);
prt_tests(lm3);

lm4=lmerror_robust_panel(ywith,xwith,W);
prt_tests(lm4);
% ----------------------------------------------------------------------------------------
% spatial and time period fixed effects + (robust) LM tests for spatial lag and spatial error model
% fixed effects, within estimator
% demeaning of the y and x variables
model=3;
[ywith,xwith,meanny,meannx,meanty,meantx]=demean(y,x,N,T,model);
results=ols(ywith,xwith);
vnames=strvcat('yield','prec','temp','edu','fert','mach','labo','seed','area','dama','irri');
prt_reg(results,vnames);
intercept=mean(y)-mean(x)*results.beta; 
sfe=meanny-meannx*results.beta-kron(en,intercept);
tfe=meanty-meantx*results.beta-kron(et,intercept);
yme = y - mean(y);
ent=ones(N*T,1);
error=y-kron(tfe,en)-kron(et,sfe)-x*results.beta-kron(ent,intercept);
rsqr1 = error'*error;
rsqr2 = yme'*yme;
FE_rsqr2 = 1.0 - rsqr1/rsqr2 % r-squared including fixed effects
sige=results.sige*((nobs-K)/nobs);
loglikstfe=-nobs/2*log(2*pi*sige)-1/(2*sige)*results.resid'*results.resid

LMsarsem_panel(results,W,ywith,xwith); % (Robust) LM tests

lm1=lmlag_panel(ywith,xwith,W);
prt_tests(lm1);

lm2=lmerror_panel(ywith,xwith,W);
prt_tests(lm2);

lm3=lmlag_robust_panel(ywith,xwith,W);
prt_tests(lm3);

lm4=lmerror_robust_panel(ywith,xwith,W);
prt_tests(lm4);
% ----------------------------------------------------------------------------------------
% Tests for the joint significance of spatial and/or time-period fixed effects
LR=-2*(logliktfe-loglikstfe);
dof=N;
probability=1-chis_prb(LR,dof);
% Note: probability > 0.05 implies rejection of spatial fixed effects
fprintf(1,'LR-test joint significance spatial fixed effects, degrees of freedom and probability = %9.4f,%6d,%9.4f \n',LR,dof,probability);
LR=-2*(logliksfe-loglikstfe);
dof=T;
probability=1-chis_prb(LR,dof);
% Note: probability > 0.05 implies rejection of spatial fixed effects
fprintf(1,'LR-test joint significance time-periode fixed effects, degrees of freedom and probability = %9.4f,%6d,%9.4f \n',LR,dof,probability);