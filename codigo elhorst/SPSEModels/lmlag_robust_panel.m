function result = lmlag_robust_panel(y,x,W)
% PURPOSE: LM robust lag test for panel data: Test for a spatially lagged dependent
%          variable when a spatial error process is present. 
% ---------------------------------------------------
%  USAGE: result = lmlag_robust_panel(y,x,W)
%  where: y = dependent variable vector
%         x = independent variables matrix
%         W = contiguity matrix (standardized)
% ---------------------------------------------------
%  RETURNS: a structure variable
%         result.meth = 'lmlag_robust_panel'
%         result.lm   = LM statistic
%         result.prob = marginal probability
%         result.chi1 = 6.635 (chi-squared 1 dof at 99% level)
%         result.nobs = # of observations
%         result.nvar = # of variables
% ---------------------------------------------------
% NOTE: lm > 6.635,  => small prob,
%                    => reject HO: of no spatial correlation
% ---------------------------------------------------
% See also:  walds, lratios, moran
% ---------------------------------------------------
% REFERENCES: Elhorst JP (2009) Spatial Panel Data Models. In Fischer MM,
% Getis A (Eds.) Handbook of Applied Spatial Analysis, pp. 377-407. 
% Springer: Berlin Heidelberg New York. http://www.springerlink.com/content/u8086626076458v0/ 
% ---------------------------------------------------

% written by:

% Donald J. Lacombe
% Research Associate Professor
% Regional Research Institute
% 886 Chestnut Ridge Road
% PO Box 6825
% Morgantown, WV 26506-6825
% donald.lacombe@mail.wvu.edu

% Based on the lmerror code of James P. LeSage
% jlesage@spatial-econometrics.com

if nargin ~= 3
error('Wrong # of arguments to lmlag_robust_panel');
end;

[N C] = size(W);
T = length(y)/N;
NT = N*T;
[junk k] = size(x); 

xpxi = (x'*x)\eye(k);      % Faster way of computing inv(x'*x)
b = xpxi*(x'*y);           % OLS coefficients
M = speye(NT) - x*xpxi*x';   % M matrix
e = M*y;                   % Calculate residuals
sighat = (e'*e)/NT;        % Calculate sigma hat

It = speye(T);
ItWn = kron(It,W);

Tw = trace(W*W + W'*W);                                  % Tw in Elhorst
term1 = [((ItWn)*x*b)'*M*(ItWn)*x*b+(T*Tw*sighat)];      
J = (1/(sighat))*term1;                                  % J term in Elhorst
lm1 = (e'*ItWn*e/sighat);
lm2 = (e'*ItWn*y/sighat);
lmr1 = (lm1 - lm2);
lmr2 = lmr1*lmr1;
den = J-T*Tw;
lmrobpan = lmr2/den;
prob = 1-chis_prb(lmrobpan,1);

result.meth = 'lmlag_robust_panel';
result.lm = lmrobpan;
result.prob = prob;
result.chi1   = 6.64;
result.nobs = NT;
result.nvar = k;
result.T    = T;

