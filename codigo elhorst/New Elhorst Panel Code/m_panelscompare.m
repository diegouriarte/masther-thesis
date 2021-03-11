% Demonstration file for Elhorst Panel Data code
%
% Dataset downloaded from www.wiley.co.uk/baltagi/
% Spatial weights matrix constructed by Elhorst
%
% written by: J.Paul Elhorst summer 2010
% University of Groningen
% Department of Economics
% 9700AV Groningen
% the Netherlands
% j.p.elhorst@rug.nl
%
% REFERENCE: 
% Elhorst JP (2010) Matlab Software for Spatial Panels. Under review.
%
% Elhorst JP (2010) Spatial Panel Data Models. In Fischer MM, Getis A (Eds.) 
% Handbook of Applied Spatial Analysis, Ch. C.2. Springer: Berlin Heidelberg New York.
%
% New:

% 1) Direct/Indirect effect esimates of the explanatory variables
% LeSage JP, Pace RK (2009) Introduction to Spatial Econometrics. Boca Raton, Taylor & Francis Group.
% routine direct_indirect_effects_estimates(results,W,spat_model) is written by J.P. Elhorst
% routines panel_effects_sar(results,vnames,W) and panel_effects_sar(results,vnames,W) 
% are written and made available by D. Lacombe
% User may use both routines (note: results are slightly different from each other since they are based on draws from a distrobution 
% or choose one particular routine. If N is large, user should choose
% Lacombe's routines, since this one is much more efficient computationally

% 2) Bias correction of coefficient estimates
% Lee Lf, Yu J. (2010) Estimation of spatial autoregressive models with
% fixed effects, Journal of Econometrics 154: 165-185.

% 3) Selection framework to determine which spatial panel data model best
% describes the data.

% dimensions of the problem
clear all;

A1=xlsread('F:\minmin\maize\���׵���.xls',1, 'B2:AE28');
A2=xlsread('F:\minmin\maize\rd.xlsx',1, 'B2:AE28');
A3=xlsread('F:\minmin\maize\ũҵ�̶��ʲ���Ͷ��.xls',1, 'B2:AE28');
A4=xlsread('F:\minmin\maize\ũ���Ͷ����Ļ�״��.xls', 1,'B2:AE28');
A5=xlsread('F:\minmin\maize\��ˮ.xlsx',1, 'B2:AE28');
A6=xlsread('F:\minmin\maize\�¶�.xlsx',1, 'B2:AE28');
A7=xlsread('F:\minmin\maize\ˮ���ܿ���.xls',1, 'B2:AE28');
A8=xlsread('F:\minmin\maize\���ʷ�.xls',1, 'B2:AE28');
A9=xlsread('F:\minmin\maize\��е��ҵ��.xls',1, 'B2:AE28');
A10=xlsread('F:\minmin\maize\����ҵ��Ա.xls',1, 'B2:AE28');
A11=xlsread('F:\minmin\maize\���ӷ�.xls',1, 'B2:AE28');
A12=xlsread('F:\minmin\maize\�������.xls',1, 'B2:AE28');
A13=xlsread('F:\minmin\maize\�������������.xlsx',1, 'B2:AE28');
A14=xlsread('F:\minmin\maize\������Ч������.xlsx',1, 'B2:AE28');
A15=xlsread('F:\minmin\maize\������Ҫũ���ﲥ�����.xlsx',1, 'B2:AE28');
A16=xlsread('F:\minmin\maize\ũ������ҵ��ҵ��Ա.xlsx',1, 'B2:AE28');
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
y=log(y);
x=log(x+1);
[y,mu1,sigma1]=zscore(y);
[x,mu2,sigma2]=zscore(x);
for t=1:T
    t1=(t-1)*N+1;t2=t*N;
    wx(t1:t2,:)=W*x(t1:t2,:);
end
xconstant=ones(N*T,1);
[nobs K]=size(x);
% ----------------------------------------------------------------------------------------
% No fixed effects + spatially lagged dependent variable
info.lflag=0; % required for exact results
info.model=0;
info.fe=0; % Do not print intercept and fixed effects; use info.fe=1 to turn on
% New routines to calculate effects estimates
results=sar_panel_FE(y,[xconstant x],W,T,info); 
vnames=strvcat('yield','intercept','prec','temp','edu','fert','mach','labo','seed','area','dama','irri');
% Print out coefficient estimates
prt_sp(results,vnames,1);
% Print out effects estimates
spat_model=0;
direct_indirect_effects_estimates(results,W,spat_model);
panel_effects_sar(results,vnames,W);
% ----------------------------------------------------------------------------------------
% No fixed effects + spatially lagged dependent variable + spatially
% independent variables
info.lflag=0; % required for exact results
info.model=0;
info.fe=0; % Do not print intercept and fixed effects; use info.fe=1 to turn on
% New routines to calculate effects estimates
results=sar_panel_FE(y,[xconstant x wx],W,T,info); 
vnames=strvcat('yield','intercept','prec','temp','edu','fert','mach','labo','seed','area','dama','irri','W*prec','W*temp','W*edu','W*fert','W*mach','W*labo','W*seed','W*area','W*dama','W*irri');
% Print out coefficient estimates
prt_sp(results,vnames,1);
% Print out effects estimates
spat_model=1;
direct_indirect_effects_estimates(results,W,spat_model);
panel_effects_sdm(results,vnames,W);
% ----------------------------------------------------------------------------------------
% Spatial fixed effects + spatially lagged dependent variable
info.lflag=0; % required for exact results
info.model=1;
info.fe=0; % Do not print intercept and fixed effects; use info.fe=1 to turn on
% New routines to calculate effects estimates
results=sar_panel_FE(y,x,W,T,info); 
vnames=strvcat('yield','prec','temp','edu','fert','mach','labo','seed','area','dama','irri');
% Print out coefficient estimates
prt_sp(results,vnames,1);
% Print out effects estimates
spat_model=0;
direct_indirect_effects_estimates(results,W,spat_model);
panel_effects_sar(results,vnames,W);
% ----------------------------------------------------------------------------------------
% Spatial fixed effects + spatially lagged dependent variable + spatially
% independent variables
info.lflag=0; % required for exact results
info.model=1;
info.fe=0; % Do not print intercept and fixed effects; use info.fe=1 to turn on
% New routines to calculate effects estimates
results=sar_panel_FE(y,[x wx],W,T,info); 
vnames=strvcat('yield','prec','temp','edu','fert','mach','labo','seed','area','dama','irri','W*prec','W*temp','W*edu','W*fert','W*mach','W*labo','W*seed','W*area','W*dama','W*irri');
% Print out coefficient estimates
prt_sp(results,vnames,1);
% Print out effects estimates
spat_model=1;
direct_indirect_effects_estimates(results,W,spat_model);
panel_effects_sdm(results,vnames,W);
% ----------------------------------------------------------------------------------------
% Time period fixed effects + spatially lagged dependent variable
info.lflag=0; % required for exact results
info.model=2;
info.fe=0; % Do not print intercept and fixed effects; use info.fe=1 to turn on
% New routines to calculate effects estimates
results=sar_panel_FE(y,x,W,T,info); 
vnames=strvcat('yield','prec','temp','edu','fert','mach','labo','seed','area','dama','irri');
% Print out coefficient estimates
prt_sp(results,vnames,1);
% Print out effects estimates
spat_model=0;
direct_indirect_effects_estimates(results,W,spat_model);
panel_effects_sar(results,vnames,W);
% ----------------------------------------------------------------------------------------
% Time period fixed effects + spatially lagged dependent variable + spatially
% independent variables
info.lflag=0; % required for exact results
info.model=2;
info.fe=0; % Do not print intercept and fixed effects; use info.fe=1 to turn on
% New routines to calculate effects estimates
results=sar_panel_FE(y,[x wx],W,T,info); 
vnames=strvcat('yield','prec','temp','edu','fert','mach','labo','seed','area','dama','irri','W*prec','W*temp','W*edu','W*fert','W*mach','W*labo','W*seed','W*area','W*dama','W*irri');
% Print out coefficient estimates
prt_sp(results,vnames,1);
% Print out effects estimates
spat_model=1;
direct_indirect_effects_estimates(results,W,spat_model);
panel_effects_sdm(results,vnames,W);
% ----------------------------------------------------------------------------------------
% Spatial and time period fixed effects + spatially lagged dependent variable
info.lflag=0; % required for exact results
info.model=3;
info.fe=0; % Do not print intercept and fixed effects; use info.fe=1 to turn on
% New routines to calculate effects estimates
results=sar_panel_FE(y,x,W,T,info); 
vnames=strvcat('yield','prec','temp','edu','fert','mach','labo','seed','area','dama','irri');
% Print out coefficient estimates
prt_sp(results,vnames,1);
% Print out effects estimates
spat_model=0;
direct_indirect_effects_estimates(results,W,spat_model);
panel_effects_sar(results,vnames,W);
% ----------------------------------------------------------------------------------------
% Spatial and time period fixed effects + spatially lagged dependent variable + spatially
% independent variables
% No bias correction
info.bc=0;
info.lflag=0; % required for exact results
info.model=3;
info.fe=0; % Do not print intercept and fixed effects; use info.fe=1 to turn on
% New routines to calculate effects estimates
results=sar_panel_FE(y,[x wx],W,T,info); 
vnames=strvcat('yield','prec','temp','edu','fert','mach','labo','seed','area','dama','irri','W*prec','W*temp','W*edu','W*fert','W*mach','W*labo','W*seed','W*area','W*dama','W*irri');
% Print out coefficient estimates
prt_sp(results,vnames,1);
% Print out effects estimates
spat_model=1;
direct_indirect_effects_estimates(results,W,spat_model);
panel_effects_sdm(results,vnames,W);
% Wald test for spatial Durbin model against spatial lag model
btemp=results.parm;
varcov=results.cov;
Rafg=zeros(K,2*K+2);
for k=1:K
    Rafg(k,K+k)=1; % R(1,3)=0 and R(2,4)=0;
end
Wald_spatial_lag=(Rafg*btemp)'*inv(Rafg*varcov*Rafg')*Rafg*btemp
prob_spatial_lag=1-chis_cdf (Wald_spatial_lag, K) % probability greater than 0.05 points to insignificance
% LR test spatial Durbin model against spatial lag model (requires
% estimation results of the spatial lag model to be available)
resultssar=sar_panel_FE(y,x,W,T,info); 
LR_spatial_lag=-2*(resultssar.lik-results.lik)
prob_spatial_lag=1-chis_cdf (LR_spatial_lag,K) % probability greater than 0.05 points to insignificance
% Wald test spatial Durbin model against spatial error model
R=zeros(K,1);
for k=1:K
    R(k)=btemp(2*K+1)*btemp(k)+btemp(K+k); % k changed in 1, 7/12/2010
%   R(1)=btemp(5)*btemp(1)+btemp(3);
%   R(2)=btemp(5)*btemp(2)+btemp(4);
end
Rafg=zeros(K,2*K+2);
for k=1:K
    Rafg(k,k)    =btemp(2*K+1); % k changed in 1, 7/12/2010
    Rafg(k,K+k)  =1;
    Rafg(k,2*K+1)=btemp(k);
%   Rafg(1,1)=btemp(5);Rafg(1,3)=1;Rafg(1,5)=btemp(1);
%   Rafg(2,2)=btemp(5);Rafg(2,4)=1;Rafg(2,5)=btemp(2);
end    
Wald_spatial_error=R'*inv(Rafg*varcov*Rafg')*R
prob_spatial_error=1-chis_cdf (Wald_spatial_error,K) % probability greater than 0.05 points to insignificance
% LR test spatial Durbin model against spatial error model (requires
% estimation results of the spatial error model to be available
resultssem=sem_panel_FE(y,x,W,T,info); 
LR_spatial_error=-2*(resultssem.lik-results.lik)
prob_spatial_error=1-chis_cdf (LR_spatial_error,K) % probability greater than 0.05 points to insignificance
% ----------------------------------------------------------------------------------------
% Spatial and time period fixed effects + spatially lagged dependent variable + spatially
% independent variables
info.lflag=0; % required for exact results
info.model=3;
info.fe=0; % Do not print intercept and fixed effects; use info.fe=1 to turn on
info.bc=1;
% New routines to calculate effects estimates
results=sar_panel_FE(y,[x wx],W,T,info); 
vnames=strvcat('yield','prec','temp','edu','fert','mach','labo','seed','area','dama','irri','W*prec','W*temp','W*edu','W*fert','W*mach','W*labo','W*seed','W*area','W*dama','W*irri');
% Print out coefficient estimates
prt_sp(results,vnames,1);
% Print out effects estimates
spat_model=1;
direct_indirect_effects_estimates(results,W,spat_model);
panel_effects_sdm(results,vnames,W);
% Wald test for spatial lag model
btemp=results.parm;
varcov=results.cov;
Rafg=zeros(K,2*K+2);
for k=1:K
    Rafg(k,K+k)=1; % R(1,3)=0 and R(2,4)=0;
end
Wald_spatial_lag=(Rafg*btemp)'*inv(Rafg*varcov*Rafg')*Rafg*btemp
prob_spatial_lag= 1-chis_cdf (Wald_spatial_lag, K) % probability greater than 0.05 points to insignificance
% LR test spatial Durbin model against spatial lag model (requires
% estimation results of the spatial lag model to be available)
resultssar=sar_panel_FE(y,x,W,T,info); 
LR_spatial_lag=-2*(resultssar.lik-results.lik)
prob_spatial_lag=1-chis_cdf (LR_spatial_lag,K) % probability greater than 0.05 points to insignificance
% Wald test for spatial error model
R=zeros(K,1);
for k=1:K
    R(k)=btemp(2*K+1)*btemp(k)+btemp(K+k); % k changed in 1, 7/12/2010
%   R(1)=btemp(5)*btemp(1)+btemp(3);
%   R(2)=btemp(5)*btemp(2)+btemp(4);
end
Rafg=zeros(K,2*K+2);
for k=1:K
    Rafg(k,k)    =btemp(2*K+1); % k changed in 1, 7/12/2010
    Rafg(k,K+k)  =1;
    Rafg(k,2*K+1)=btemp(k);
%   Rafg(1,1)=btemp(5);Rafg(1,3)=1;Rafg(1,5)=btemp(1);
%   Rafg(2,2)=btemp(5);Rafg(2,4)=1;Rafg(2,5)=btemp(2);
end    
Wald_spatial_error=R'*inv(Rafg*varcov*Rafg')*R
prob_spatial_error= 1-chis_cdf (Wald_spatial_error,K) % probability greater than 0.05 points to insignificance
% LR test spatial Durbin model against spatial error model (requires
% estimation results of the spatial error model to be available
resultssem=sem_panel_FE(y,x,W,T,info); 
LR_spatial_error=-2*(resultssem.lik-results.lik)
prob_spatial_error=1-chis_cdf (LR_spatial_error,K) % probability greater than 0.05 points to insignificance
% needed for Hausman test later on
logliklag=results.lik; 
blagfe=results.parm(1:end-1);
covblagfe=results.cov(1:end-1,1:end-1);
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% random effects estimator by ML %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% 
% Spatial random effects and time period fixed effects + spatially lagged dependent variable + spatially
% independent variables
[ywith,xwith,meanny,meannx,meanty,meantx]=demean(y,[x wx],N,T,2); % 2=time dummies
info.model=1;
results=sar_panel_RE(ywith,xwith,W,T,info); 
prt_sp(results,vnames,1);
% Print out effects estimates
spat_model=1;
direct_indirect_effects_estimates(results,W,spat_model);
panel_effects_sdm(results,vnames,W);
% Wald test for spatial lag model
btemp=results.parm(1:2*K+2);
varcov=results.cov(1:2*K+2,1:2*K+2);
Rafg=zeros(K,2*K+2);
for k=1:K
    Rafg(k,K+k)=1; % R(1,3)=0 and R(2,4)=0;
end
Wald_spatial_lag=(Rafg*btemp)'*inv(Rafg*varcov*Rafg')*Rafg*btemp
prob_spatial_lag= 1-chis_cdf (Wald_spatial_lag, K) % probability greater than 0.05 points to insignificance
% Wald test for spatial error model
R=zeros(K,1);
for k=1:K
    R(k)=btemp(2*K+1)*btemp(k)+btemp(K+k); % k changed in 1, 7/12/2010
%   R(1)=btemp(5)*btemp(1)+btemp(3);
%   R(2)=btemp(5)*btemp(2)+btemp(4);
end
Rafg=zeros(K,2*K+2);
for k=1:K
    Rafg(k,k)    =btemp(2*K+1); % k changed in 1, 7/12/2010
    Rafg(k,K+k)  =1;
    Rafg(k,2*K+1)=btemp(k);
%   Rafg(1,1)=btemp(5);Rafg(1,3)=1;Rafg(1,5)=btemp(1);
%   Rafg(2,2)=btemp(5);Rafg(2,4)=1;Rafg(2,5)=btemp(2);
end    
Wald_spatial_error=R'*inv(Rafg*varcov*Rafg')*R
prob_spatial_error= 1-chis_cdf (Wald_spatial_error,K) % probability greater than 0.05 points to insignificance
% needed for Hausman test later on
logliklagre=results.lik;
blagre=results.parm(1:end-2);
covblagre=results.cov(1:end-2,1:end-2);
% ----------------------------------------------------------------------------------------
% Hausman test FE versus RE 
hausman=(blagfe-blagre)'*inv(covblagre-covblagfe)*(blagfe-blagre);
dof=length(blagfe);
probability=1-chis_prb(abs(hausman),dof); 
% Note: probability < 0.025 implies rejection of random effects model in favor of fixed effects model
% Use 0.025, since it is a one-sided test
fprintf(1,'Hausman test-statistic, degrees of freedom and probability = %9.4f,%6d,%9.4f \n',abs(hausman),dof,probability);