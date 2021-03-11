% dimensions of the problem

A = xlsread('datos_diesel_full.xlsx');
W1 = xlsread('matriz-distancia-diesel-full.xlsx');
T=18; % number of time periods
N=430; % number of regions
% row-normalize W

W=normw(W1); % function of LeSage
y=A(:,[1]); % column number in the data matrix that corresponds to the dependent variable
x=A(:,[2,3,4,5]); % column numbers in the data matrix that correspond to the independent variables
for t=1:T
    t1=(t-1)*N+1;t2=t*N;
    wx(t1:t2,:)=W*x(t1:t2,:);
end
xconstant=ones(N*T,1);
[nobs K]=size(x);



%% 
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
vnames=strvcat('pventa','comprada','suministrada','vecina','sc','W*comprada','W*suministrada', ...
                'W*vecina', 'W*sc');
% vnames=strvcat('logcit','logp','logy','W*logp','W*logy');
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