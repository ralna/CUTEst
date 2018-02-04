function [varargout] = cutest_connames( varargin )
% Return constraint names.
% Usage: cnames = cutest_connames()
    varargout = cell(1,nargout);
    [varargout{:}] = mcutest('connames',varargin{:});
