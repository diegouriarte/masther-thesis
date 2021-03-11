% PURPOSE: Demonstration file for Elhorst Spatial Panel Models
%          using the LeSage and Pace effects estimates code
%---------------------------------------------------
% Effects estimates added by Donald J. Lacombe
% Donald J. Lacombe
% Research Associate Professor
% Regional Research Institute
% 886 Chestnut Ridge Road
% PO Box 6825
% Morgantown, WV 26506-6825
% donald.lacombe@mail.wvu.edu
% http://www.rri.wvu.edu/lacombe/~lacombe.htm
%
% REFERENCES:
% Elhorst JP (2010) Matlab Software for Spatial Panels. Under review.
%

% dimensions of the problem
A=wk1read('cigarette.wk1',1,0);
W1=wk1read('spat-sym-us.wk1');
T=30; % number of time periods
N=46; % number of regions
% row-normalize W
W=normw(W1); % function of LeSage
y=A(:,[3]); % column number in the data matrix that corresponds to the dependent variable
x=A(:,[4,6]); % column numbers in the data matrix that correspond to the independent variables
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
info.fe=0; % no print intercept and spatial fixed effects
% New routines to calculate effects estimates
results=sar_panel_FE(y,[xconstant x],W,T,info); 
vnames=strvcat('logcit','intercept','logp','logy');
% Print out coefficient estimates
prt_sp(results,vnames,1);
% Print out effects estimates
panel_effects_sar(results,vnames,W);
pause
% ----------------------------------------------------------------------------------------
% No fixed effects + spatially lagged dependent variable + spatially
% independent variables
info.lflag=0; % required for exact results
info.model=0;
info.fe=0; % no print intercept and spatial fixed effects
% New routines to calculate effects estimates
results=sar_panel_FE(y,[xconstant x wx],W,T,info); 
vnames=strvcat('logcit','intercept','logp','logy','W*logp','W*logy');
% Print out coefficient estimates
prt_sp(results,vnames,1);
% Print out effects estimates
panel_effects_sdm(results,vnames,W);
pause
% ----------------------------------------------------------------------------------------
% Spatial fixed effects + spatially lagged dependent variable
info.lflag=0; % required for exact results
info.model=1;
info.fe=0; % no print intercept and spatial fixed effects
% New routines to calculate effects estimates
results=sar_panel_FE(y,x,W,T,info); 
vnames=strvcat('logcit','logp','logy');
% Print out coefficient estimates
prt_sp(results,vnames,1);
% Print out effects estimates
panel_effects_sar(results,vnames,W);
pause
% ----------------------------------------------------------------------------------------
% Spatial fixed effects + spatially lagged dependent variable + spatially
% independent variables
info.lflag=0; % required for exact results
info.model=1;
info.fe=0; % no print intercept and spatial fixed effects
% New routines to calculate effects estimates
results=sar_panel_FE(y,[x wx],W,T,info); 
vnames=strvcat('logcit','logp','logy','W*logp','W*logy');
% Print out coefficient estimates
prt_sp(results,vnames,1);
% Print out effects estimates
panel_effects_sdm(results,vnames,W);
pause
% ----------------------------------------------------------------------------------------
% Time period fixed effects + spatially lagged dependent variable
info.lflag=0; % required for exact results
info.model=2;
info.fe=0; % no print intercept and spatial fixed effects
% New routines to calculate effects estimates
results=sar_panel_FE(y,x,W,T,info); 
vnames=strvcat('logcit','logp','logy');
% Print out coefficient estimates
prt_sp(results,vnames,1);
% Print out effects estimates
panel_effects_sar(results,vnames,W);
pause
% ----------------------------------------------------------------------------------------
% Time period fixed effects + spatially lagged dependent variable + spatially
% independent variables
info.lflag=0; % required for exact results
info.model=2;
info.fe=0; % no print intercept and spatial fixed effects
% New routines to calculate effects estimates
results=sar_panel_FE(y,[x wx],W,T,info); 
vnames=strvcat('logcit','logp','logy','W*logp','W*logy');
% Print out coefficient estimates
prt_sp(results,vnames,1);
% Print out effects estimates
panel_effects_sdm(results,vnames,W);
pause
% ----------------------------------------------------------------------------------------
% Spatial and time period fixed effects + spatially lagged dependent variable
info.lflag=0; % required for exact results
info.model=3;
info.fe=0; % no print intercept and spatial fixed effects
% New routines to calculate effects estimates
results=sar_panel_FE(y,x,W,T,info); 
vnames=strvcat('logcit','logp','logy');
% Print out coefficient estimates
prt_sp(results,vnames,1);
% Print out effects estimates
panel_effects_sar(results,vnames,W);
pause
% ----------------------------------------------------------------------------------------
% Spatial and time period fixed effects + spatially lagged dependent variable + spatially
% independent variables
% No bias correction
info.bc=0;
info.lflag=0; % required for exact results
info.model=3;
info.fe=0; % no print intercept and spatial fixed effects
% New routines to calculate effects estimates
results=sar_panel_FE(y,[x wx],W,T,info); 
vnames=strvcat('logcit','logp','logy','W*logp','W*logy');
% Print out coefficient estimates
prt_sp(results,vnames,1);
% Print out effects estimates
panel_effects_sdm(results,vnames,W);
pause
% ----------------------------------------------------------------------------------------
% Spatial and time period fixed effects + spatially lagged dependent variable + spatially
% independent variables
info.lflag=0; % required for exact results
info.model=3;
info.fe=0; % no print intercept and spatial fixed effects
info.bc=1;
% New routines to calculate effects estimates
results=sar_panel_FE(y,[x wx],W,T,info); 
vnames=strvcat('logcit','logp','logy','W*logp','W*logy');
% Print out coefficient estimates
prt_sp(results,vnames,1);
% Print out effects estimates
panel_effects_sdm(results,vnames,W);
pause
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% random effects estimator by ML %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% 
% Spatial random effects and time period fixed effects + spatially lagged dependent variable + spatially
% independent variables
[ywith,xwith,meanny,meannx,meanty,meantx]=demean(y,[x wx],N,T,2); % 2=time dummies
info.model=1;
results=sar_panel_RE(ywith,xwith,W,T,info); 
prt_sp(results,vnames,1);
% Print out effects estimates
panel_effects_sdm(results,vnames,W);
pause
% ----------------------------------------------------------------------------------------
% Spatial and time period fixed effects + spatially lagged dependent variable + spatially
% independent variables
% No bias correction
info.bc=0;
info.lflag=0; % required for exact results
info.model=3;
info.fe=0; % no print intercept and spatial fixed effects
% New routines to calculate effects estimates
results=sar_panel_FE(y,[x wx],W,T,info); 
vnames=strvcat('logcit','logp','logy','W*logp','W*logy');
% Print out coefficient estimates
prt_sp(results,vnames,1);
% Print out effects estimates
panel_effects_sdm(results,vnames,W);
pause
% ----------------------------------------------------------------------------------------
% Spatial and time period fixed effects + spatially lagged dependent variable + spatially
% independent variables
info.lflag=0; % required for exact results
info.model=3;
info.fe=0; % no print intercept and spatial fixed effects
info.bc=1;
% New routines to calculate effects estimates
results=sar_panel_FE(y,[x wx],W,T,info); 
vnames=strvcat('logcit','logp','logy','W*logp','W*logy');
% Print out coefficient estimates
prt_sp(results,vnames,1);
% Print out effects estimates
panel_effects_sdm(results,vnames,W);
pause
% ----------------------------------------------------------------------------------------
% Spatial and time period fixed effects + spatially lagged dependent variable
info.lflag=0; % required for exact results
info.model=3;
info.fe=0; % no print intercept and spatial fixed effects
% New routines to calculate effects estimates
results=sar_panel_RE(y,x,W,T,info); 
vnames=strvcat('logcit','logp','logy');
% Print out coefficient estimates
prt_sp(results,vnames,1);
% Print out effects estimates
panel_effects_sar(results,vnames,W);