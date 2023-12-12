function [varargout] = cutest_cons( varargin )
% Return constraint bodies and Jacobian if requested.
% or return a single constraint value and its gradient if requested
% Usage:  c = cutest_cons(x)    or  [c,J]   = cutest_cons(x)
%        ci = cutest_cons(x,i)  or  [ci,gi] = cutest_cons(x,i)
    varargout = cell(1,nargout);
    [varargout{:}] = ocutest('cons',varargin{:});
