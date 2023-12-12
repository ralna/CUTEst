function [varargout] = cutest_lagjac( varargin )
% Return the gradient of the objective or Lagrangian and Jacobian
% [g,J] = cutest_lagjac(x)  or  [g,J] = cutest_lagjac(x,v)
    varargout = cell(1,nargout);
    [varargout{:}] = ocutest('lagjac',varargin{:});
