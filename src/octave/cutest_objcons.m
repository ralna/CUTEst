function [varargout] = cutest_objcons( varargin )

% Evaluate objective function value and constraint bodies.
% Usage:  [f,c] = cutest_objcons(x)
    varargout = cell(1,nargout);
    [varargout{:}] = ocutest('objcons',varargin{:});

