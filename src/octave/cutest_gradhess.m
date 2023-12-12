function [varargout] = cutest_gradhess( varargin )
% Return the Hessian of the Lagrangian, the Jacobian of the constraints
% and the gradient of either the objective function or the Lagrangian
% Usage:  [g,H] = cutest_gradhess( x )   if the problem is unconstrained, or
%       [g,J,H] = cutest_gradhess( x, v, gradf, jtrans )  if it is constrained
%                 where gradf and jtrans are either true or false;
%                  gradf = true : returns the gradient of the objective in g
%                          false: returns the gradient of the Lagrangian in g
%                 jtrans = true : returns the transpose Jacobian in J
%                 jtrans = false: returns the Jacobian in J
    varargout = cell(1,nargout);
    [varargout{:}] = ocutest('gradhess',varargin{:});
