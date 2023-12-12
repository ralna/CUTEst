function [varargout] = cutest_terminate( varargin )
% Remove existing internal workspace
% Usage: cutest_terminate()

    varargout = cell(1,nargout);
    [varargout{:}] = ocutest('terminate',varargin{:});
