function [varargout] = cutest_varnames( varargin )
% Return variable names.
% Usage: vnames = cutest_varnames()
    varargout = cell(1,nargout);
    [varargout{:}] = ocutest('varnames',varargin{:});
