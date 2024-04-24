function [value,isterminal,direction] = stopevent(tt,xx,Ap,Aorifice,Cd,rho,speed,S)

% Simple file to stop and abort the odes that take too much time for a
% particular parameterization. It is more useful for calibration, but I
% keep it for now as I wish to try a simple calibration later.

if toc > 30*60 % time in seconds
    value = 0;
    isterminal = 1;     % stop the integration
    direction = 0;      % all events
else
    value = 1;
    isterminal = 0;     % don't stop
    direction = 0;      % all events
end