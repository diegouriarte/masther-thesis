function prt_tests(results,vnames,fid)
% PURPOSE: Prints output using spatial regression results structures
%---------------------------------------------------
% USAGE: prt_spat(results,vnames,fid)
% Where: results = a structure returned by a spatial regression 
%        vnames  = an optional vector of variable names
%        fid     = optional file-id for printing results to a file
%                  (defaults to the MATLAB command window)
%--------------------------------------------------- 
%  NOTES: e.g. vnames = strvcat('y','const','x1','x2');
%         e.g. fid = fopen('ols.out','wr');
%  use prt_spat(results,[],fid) to print to a file with no vnames               
% --------------------------------------------------
%  RETURNS: nothing, just prints the spatial regression results
% --------------------------------------------------
% SEE ALSO: prt, plt
%---------------------------------------------------   

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if ~isstruct(results)
 error('prt_spat requires structure argument');
elseif nargin == 1
 nflag = 0; fid = 1;
elseif nargin == 2
 fid = 1; nflag = 1;
elseif nargin == 3
 nflag = 0;
 [vsize junk] = size(vnames); % user may supply a blank argument
   if vsize > 0
   nflag = 1;          
   end;
else
 error('Wrong # of arguments to prt_spat');
end;

switch results.meth


case{'moran'}
fprintf(fid,'Moran I-test for spatial correlation in residuals \n');
variable = ' '; 
in.rnames = strvcat(variable,'Moran I','Moran I-statistic','Marginal Probability', ...
         'mean','standard deviation');
in.fmt = '%16.8f';
tmp = sqrt(results.ivar);
mat = [results.morani
       results.istat
       results.prob
       results.imean
       tmp];
mprint(mat,in);
return;    

case{'lratios'}
fprintf(fid,'LR test for spatial correlation in residuals \n');
variable = ' ';
in.rnames = strvcat(variable,'LR value','Marginal Probability','chi-squared(1) value');
in.fmt = '%16.8f';
mat = [results.lratio
       results.prob
       results.chi1];
mprint(mat,in);
return;    

case{'lmerror'}
fprintf(fid,'LM error test for spatial correlation in residuals \n');
variable = ' ';
in.rnames = strvcat(variable,'LM value','Marginal Probability','chi(1) .01 value');
in.fmt = '%16.8f';
mat = [results.lm
       results.prob
       results.chi1];
mprint(full(mat),in);
return;    

case{'lmsar'}
fprintf(fid,'LM error test for spatial correlation in SAR model residuals \n');
variable = ' ';
in.rnames = strvcat(variable,'LM value','Marginal Probability','chi(1) .01 value');
in.fmt = '%16.8f';
mat = [results.lm
       results.prob
       results.chi1];
mprint(full(mat),in);
return;    

case{'walds'}
fprintf(fid,'Wald test for spatial correlation in residuals \n');
variable = ' ';
in.rnames = strvcat(variable,'Wald value','Marginal Probability','chi(1) .01 value');
in.fmt = '%16.8f';
mat = [results.wald
       results.prob
       results.chi1];
mprint(full(mat),in);
return; 

case{'lmlag'} % LM lag output
fprintf(fid,'LM lag test for omitted spatial lag \n');
variable = ' ';
in.rnames = strvcat(variable,'LM value','Marginal Probability','chi(1) .01 value');
in.fmt = '%16.8f';
mat = [results.lm
       results.prob
       results.chi1];
mprint(full(mat),in);
return;  

case{'lmlag_robust'} % LM lag robust output
fprintf(fid,'Robust LM lag test \n');
variable = ' ';
in.rnames = strvcat(variable,'LM value','Marginal Probability','chi(1) .01 value');
in.fmt = '%16.8f';
mat = [results.lm
       results.prob
       results.chi1];
mprint(full(mat),in);
return;    

case{'lmerror_robust'} % LM error robust output
fprintf(fid,'Robust LM error test \n');
variable = ' ';
in.rnames = strvcat(variable,'LM value','Marginal Probability','chi(1) .01 value');
in.fmt = '%16.8f';
mat = [results.lm
       results.prob
       results.chi1];
mprint(full(mat),in);
return;    

case{'lmlagerr'} % LM Lag and Error output
fprintf(fid,'Combined LM Lag and LM Error test \n');
variable = ' ';
in.rnames = strvcat(variable,'LM value','Marginal Probability','chi(2) .01 value');
in.fmt = '%16.8f';
mat = [results.lm
       results.prob
       results.chi1];
mprint(full(mat),in);
return;    

case{'lmsec'} % LM Spatial Error Components
fprintf(fid,'LM Spatial Error Components Test \n');
variable = ' ';
in.rnames = strvcat(variable,'LM value','Marginal Probability','chi(1) .01 value');
in.fmt = '%16.8f';
mat = [results.lm
       results.prob
       results.chi1];
mprint(full(mat),in);
return;    

case{'spatial_hausman'} % Spatial Hausman Tests
fprintf(fid,'Spatial Hausman Test \n');
variable = ' ';
in.rnames = strvcat(variable,'Spatial Hausman value','Marginal Probability','chi(k) significance value');
in.fmt = '%16.8f';
mat = [results.sh
       results.prob
       results.chi1];
mprint(full(mat),in);
return;    

case{'lmlag_panel'} % LM lag panel output
fprintf(fid,'LM lag test for omitted spatial lag in panel data\n');
variable = ' ';
in.rnames = strvcat(variable,'LM value','Marginal Probability','chi(1) .01 value');
in.fmt = '%16.8f';
mat = [results.lm
       results.prob
       results.chi1];
mprint(full(mat),in);
return;  

case{'lmerror_panel'} % LM error panel output
fprintf(fid,'LM error test for spatial errors in panel data\n');
variable = ' ';
in.rnames = strvcat(variable,'LM value','Marginal Probability','chi(1) .01 value');
in.fmt = '%16.8f';
mat = [results.lm
       results.prob
       results.chi1];
mprint(full(mat),in);
return;  

case{'lmlag_robust_panel'} % LM lag robust panel output
fprintf(fid,'Robust LM lag test for omitted spatial lag in panel data\n');
variable = ' ';
in.rnames = strvcat(variable,'LM value','Marginal Probability','chi(1) .01 value');
in.fmt = '%16.8f';
mat = [results.lm
       results.prob
       results.chi1];
mprint(full(mat),in);
return;  

case{'lmerror_robust_panel'} % LM error robust panel output
fprintf(fid,'Robust LM error test for spatial errors in panel data\n');
variable = ' ';
in.rnames = strvcat(variable,'LM value','Marginal Probability','chi(1) .01 value');
in.fmt = '%16.8f';
mat = [results.lm
       results.prob
       results.chi1];
mprint(full(mat),in);
return;  

otherwise
error('results structure not known by prt_spat function');
end;



