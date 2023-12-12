function [varargout] = cutest_jprod( varargin )
% Return the product of the Jacobian at x with a vector p.
% Usage:  r = cutest_jprod( x, p )  --> recomputes J(x)
%         r = cutest_jprod( p )     --> assumes J(x) was computed previously
    varargout = cell(1,nargout);
    [varargout{:}] = ocutest('Jprod',varargin{:});
