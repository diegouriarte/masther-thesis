function result = lmerror_panel(y,x,W)
% PURPOSE: LM error statistic for spatial correlation in residuals
%          of a panel regression model
% ---------------------------------------------------
%  USAGE: result = lmerror_panel(y,x,W)
%  where: y = dependent variable vector
%         x = independent variables matrix
%         W = contiguity matrix (standardized)
% ---------------------------------------------------
%  RETURNS: a structure variable
%         result.meth = 'lmerror_panel'
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
error('Wrong # of arguments to lmerror_panel');
end;

[N C] = size(W);
T = length(y)/N;
NT = N*T;
[junk k] = size(x); 

xpxi = (x'*x)\eye(k);      % Faster way of computing inv(x'*x)
M = speye(NT) - x*xpxi*x';   % M matrix
e = M*y;                   % Calculate residuals
sighat = (e'*e)/NT;        % Calculate sigma hat

It = speye(T);
ItWn = kron(It,W);

Tw = trace(W*W + W'*W);            % Tw in Elhorst
lm1 = (e'*(ItWn)*e)/sighat;        % Equation (11) Elhorst
lmerror = (lm1*lm1)*(1/(T*Tw));    % Equation (11) Elhorst
prob = 1-chis_prb(lmerror,1);


result.meth = 'lmerror_panel';
result.lm = lmerror;
result.prob = prob;
result.chi1   = 6.64;
result.nobs = NT;
result.nvar = k;
result.T   = T;

