function [varargout] = cutest_dims(varargin)
% Return problem dimensions
% Usage: [nvar,ncon] = cutest_dims()
    varargout = cell(1,nargout);
    [varargout{:}] = mcutest('dims',varargin{:});
