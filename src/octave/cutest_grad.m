function [varargout] = cutest_grad( varargin )
% Return objective function gradient or gradient of i-th constraint
% Usage:  g = cutest_grad(x) or g = cutest_grad(x,i)
    varargout = cell(1,nargout);
    [varargout{:}] = ocutest('grad',varargin{:});
