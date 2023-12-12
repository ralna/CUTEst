function [varargout] = cutest_ihess( varargin )
% Return the dense Hessian of the objective or of a constraint. The
% function index is ignored if the problem is unconstrained.
% Usage:  Hi = cutest_ihess( x, i ).
    varargout = cell(1,nargout);
    [varargout{:}] = ocutest('ihess',varargin{:});
