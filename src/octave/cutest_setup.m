function [varargout] = cutest_setup( varargin )

% Set up main problem structure
% Usage:  prob = cutest_setup()  or
%         prob = cutest_setup( derivative_type );
% where derivative_type = 0 (no derivatives required), 
%                         1 (first derivatives required)
%                         2 (first and derivatives required, default)
    varargout = cell(1,nargout);
    [varargout{:}] = ocutest('setup',varargin{:});


