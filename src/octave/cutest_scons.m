function [varargout] = cutest_scons( varargin )
% Return constraint bodies and sparse Jacobian
% or return a single constraint value and its gradient in sparse format
% Usage: [c,J] = cutest_scons(x)
%        [ci, sgci] = cutest_scons( prob.x, i )
    varargout = cell(1,nargout);
    [varargout{:}] = ocutest('scons',varargin{:});
