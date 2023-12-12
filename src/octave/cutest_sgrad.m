function [varargout] = cutest_sgrad( varargin )
% Return sparse objective function gradient or
%   sparse gradient of i-th constraint
% Usage:  sg = cutest_sgrad(x) or sg = cutest_sgrad(x,i)
    varargout = cell(1,nargout);
    [varargout{:}] = ocutest('sgrad',varargin{:});
