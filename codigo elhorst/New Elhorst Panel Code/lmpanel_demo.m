% Create Panel Data Sets
% Demonstrate the Lagrange Multiplier Tests

% written by:
% Donald J. Lacombe
% Research Associate Professor
% Regional Research Institute
% 886 Chestnut Ridge Road
% PO Box 6825
% Morgantown, WV 26506-6825
% donald.lacombe@mail.wvu.edu

clear all
clc

N=500;          % Number of observations in a cross section
T=5;            % Number of time periods
NT=N*T;         % Total number of observations

k=5;            % Number of independent variables

beta=ones(k,1)*3;   % Set coefficients to 3
sige=.7;            % sigma^2 = .7
x=randn(NT,k);
e=randn(NT,1)*sqrt(sige);
lat=randn(N,1);
long=randn(N,1);
w=make_neighborsw(lat,long,5);
rho=.7;
lambda=.7;
In=speye(N);
It=speye(T);
InWn=kron(In,w);
A=inv(In-rho*w);
ItA=kron(It,A);

% Create OLS panel data
% Run LM tests

y=x*beta+e;

lm1=lmlag_panel(y,x,w);
prt_tests(lm1);

lm2=lmerror_panel(y,x,w);
prt_tests(lm2);

lm3=lmlag_robust_panel(y,x,w);
prt_tests(lm3);

lm4=lmerror_robust_panel(y,x,w);
prt_tests(lm4);

pause 
clc

% Create SAR Panel Data
% Run LM Tests

y=ItA*x*beta+ItA*e;

lm1=lmlag_panel(y,x,w);
prt_tests(lm1);

lm2=lmerror_panel(y,x,w);
prt_tests(lm2);

lm3=lmlag_robust_panel(y,x,w);
prt_tests(lm3);

lm4=lmerror_robust_panel(y,x,w);
prt_tests(lm4);

pause 
clc

% Create SEM Panel Data
% Run LM Tests

y=x*beta+ItA*e;

lm1=lmlag_panel(y,x,w);
prt_tests(lm1);

lm2=lmerror_panel(y,x,w);
prt_tests(lm2);

lm3=lmlag_robust_panel(y,x,w);
prt_tests(lm3);

lm4=lmerror_robust_panel(y,x,w);
prt_tests(lm4);

