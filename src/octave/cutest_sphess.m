function [varargout] = cutest_sphess( varargin )
% Return the sparse Hessian of the objective function if the problem is
% unconstrained or of the Lagrangian if the problem is constrained.
% If the problem is constrained and the user wants the Hessian of the
% objective, they should call (sp)ihess().
% Usage:  H = cutest_sphess( x ) if the problem has no general constraints, or
%         H = cutest_sphess( x, v ) otherwise.
    varargout = cell(1,nargout);
    [varargout{:}] = ocutest('sphess',varargin{:});
