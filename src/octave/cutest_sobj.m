function [varargout] = cutest_sobj( varargin )
% Return function value and sparse gradient if requested.
% Usage:  f = cutest_sobj(x)  or  [f,sg] = cutest_sobj(x)
    varargout = cell(1,nargout);
    [varargout{:}] = ocutest('sobj',varargin{:});
