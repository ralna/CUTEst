function [varargout] = cutest_hprod( varargin )
% Return the matrix-vector product between the Hessian of the
% Lagrangian (or of the objective if problem is unconstrained) and a
% given vector p
% Usage:  r = cutest_hprod( x, v, p )   (Re)computes the Hessian at (x,v)
%         r = cutest_hprod( x, p )      Same, for unconstrained problems
%         r = cutest_hprod( p )         assumes H(x,v) was computed previously
    varargout = cell(1,nargout);
    [varargout{:}] = ocutest('hprod',varargin{:});
