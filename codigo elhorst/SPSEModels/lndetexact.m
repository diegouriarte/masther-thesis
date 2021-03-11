function out=lndetexact(W,lmin,lmax)
% PURPOSE: computes Pace and Barry's grid for log det(I-rho*W)
% -----------------------------------------------------------------------
% USAGE: out = lndetfull(W,lmin,lmax)
% where:    
%             W     = symmetric spatial weight matrix (standardized)
%             lmin  = lower bound on rho
%             lmax  = upper bound on rho
% -----------------------------------------------------------------------
% RETURNS: out = a structure variable
%          out.lndet = a vector of log determinants for 0 < rho < 1
%          out.rho   = a vector of rho values associated with lndet values
% -----------------------------------------------------------------------
% NOTES: should use 1/lambda(max) to 1/lambda(min) for all possible rho values
% -----------------------------------------------------------------------
% References: % R. Kelley Pace and  Ronald Barry. 1997. ``Quick
% Computation of Spatial Autoregressive Estimators'', Geographical Analysis
% -----------------------------------------------------------------------

% rewritten version of lndetfull by 
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

% rewritten by J.Paul Elhorst

rvec = lmin:.01:lmax;
[n junk] = size(W);
niter = length(rvec);
dettmp = zeros(niter,2);
for i=1:niter;
    rho = rvec(i);
    z = eye(n) - rho*W;
    [l,u] = lu(z);
    dettmp(i,1) = rho;
    dettmp(i,2) = sum(log(abs(diag(u))));
end;

out.lndet = dettmp(:,2);
out.rho = dettmp(:,1);