function [varargout] = cutest_slagjac( varargin )
% Return the sparse Jacobian and gradient of either the objective
% function or the Lagrangian.
% Usage:  [g,J] = cutest_slagjac(x) or [g,J] = cutest_slagjac(x,v)
    varargout = cell(1,nargout);
    [varargout{:}] = ocutest('slagjac',varargin{:});
