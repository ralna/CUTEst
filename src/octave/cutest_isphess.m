function [varargout] = cutest_isphess( varargin )
% Return the sparse Hessian of the objective or of a constraint. The
% function index is ignored if the problem is unconstrained.
% Usage:  Hi = cutest_isphess( x, i ).
    varargout = cell(1,nargout);
    [varargout{:}] = ocutest('isphess',varargin{:});
