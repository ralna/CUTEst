function [varargout] = cutest_obj( varargin )

% Return function value and gradient if requested.
% Usage:  f = cutest_obj(x)  or  [f,g] = cutest_obj(x)
    
    varargout = cell(1,nargout);
    [varargout{:}] = ocutest('obj',varargin{:});
