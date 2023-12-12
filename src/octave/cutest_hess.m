function [varargout] = cutest_hess( varargin )
% Return the dense Hessian of the objective function if the problem is
% unconstrained or of the Lagrangian if the problem is constrained.
% If the problem is constrained and the user wants the Hessian of the
% objective, they should call (sp)ihess().
% Usage:  H = cutest_hess( x ) if the problem has no general constraints, or
%         H = cutest_hess( x, v ) otherwise.
    varargout = cell(1,nargout);
    [varargout{:}] = ocutest('hess',varargin{:});
