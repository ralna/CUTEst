function [varargout] = cutest_lag( varargin )
% Return Lagrangian function value and gradient if requested.
% Usage:  f = cutest_lag(x,y)  or  [f,g] = cutest_lag(x,y)
    varargout = cell(1,nargout);
    [varargout{:}] = ocutest('lag',varargin{:});
