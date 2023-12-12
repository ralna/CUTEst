function [varargout] = cutest_jtprod( varargin )
% Return the product of the transpose Jacobian at x with a vector p.
% Usage:  r = cutest_jtprod( x, p )  --> recomputes J(x)
%         r = cutest_jtprod( p )     --> assumes J(x) was computed previously
    varargout = cell(1,nargout);
    [varargout{:}] = ocutest('Jtprod',varargin{:});
