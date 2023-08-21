function [output] = calibration_file2_cilinder_wood(p)

%% Calibration %%

q(1)  = 0.48*1000;      % Stick density (400 [kg/m3], Nelson, 2000)
q(2)  = 0.10;           % Length of stick [0.41 m]
q(3)  = 0.0350;         % Radius of stick [0.0250 m]
q(4)  = 0.85;           % Stick emissivity [0.85]
q(5)  = 60*60*5.67e-8;  % Stephan-Boltzmann constant [5.67e-8 J/m2/K4/(30min)]
q(6)  = 0.95;           % Emissivity of the ground [0.95]
q(7)  = 0.965;          % Emissivity of the vegetation [0.965]
q(8)  = 1.2;            % Fit parameter 1 [1.2]
q(9)  = 3.0;            % Fit parameter 2 [3.0]
q(10) = 0.5;            % Fit exponent 3  [0.5]
q(11) = 46.5;           % Climatological value [46.5 cm K/hPa]
q(12) = 0.26;           % Constant based on cloud type [0.26]
q(13) = 0.65;           % Stick Albedo [0.65]
q(14) = 0.185;          % Ground albedo [0.185]
q(15) = 1.093;          % Density of air [1.093 kg/m3]
q(16) = 1005;           % Specific heat of air [1005 J/kg/K]
q(17) = 60*60*1.9e-5;   % Thermal diffusivity of the air [1.9 × 10^-5 m2/(30min)]
q(18) = 1.51e-5;        % Kinematic viscosity of air [1.51 × 10^-5 m2/s]
q(19) = 0.018;          % Molecular mass of water [0.018 kg mol-1]
q(20) = 8.314e-3;       % Gas constant [8.314e-3 m^3 kPa K-1 mol-1]
q(21) = 0.41;           % In pine, a specific gravity of 0.41 is low density 
                        % while one with a ratio of 0.62 is considered to be 
                        % very high.) [B. Zobel, in Encyclopedia of Forest Sciences, 2004]
q(22) = 4200;           % Specific heat of water [4200 J/K/kg]
q(24) = 0.5;            % Vegetation contribution coefficient [0:1] 0.5**
q(25) = 1000;           % Density of water [1000 kg/m3]

%% Forcing variables

b       = p(7); 
[p1,sv] = forcing_minutes_cilinder(b,p);
q(23)   = sv; 
p(6)    = 1.081;

if b == 1 % DRO
    b1 = 1;  % 
    a1 = 35305; % 33550
elseif b == 2 % HQ_AWC
    b1 = 1; % 28831 DRO 2237 MLES 2253 MLRF 2513 PNW 6981 STCK 2464 HQ_AWC
    a1 = 35305; % 33550 DRO 19809 MLES 22351 MLRF 10516 PNW STCK 19983 HQ_AWC
elseif b == 3 % MLES
    b1 = 1; % 1
    a1 = 35305; % 33550
elseif b == 4 % MLRF
    b1 = 1; % 1
    a1 = 35305; % 33550
elseif b == 5 % PNW
    b1 = 1; % 2513 PNW 6981 STCK 2464 HQ_AWC
    a1 = 35305; % 10516 PNW STCK 19983 HQ_AWC
else % STCK
    b1 = 1; % 1
    a1 = 35305; % 33550
end

T_a         = p1(b1:a1,1);    % Air Temperature [K]
L_dn        = p1(b1:a1,2);    % Downwelling longwave radiation (J/h/m2)
K_d_diff    = p1(b1:a1,3);    % Diffuse shor twave radiation [J/h/m2]
K_d_dir     = p1(b1:a1,4);    % Direct short wave radiation [J/h/m2]
k_d         = p1(b1:a1,5);    % Total short wave radiation [J/h/m2]
alp_sha     = p1(b1:a1,6);    % Area of the shadow cast by the stick on a horizontal plane [m2]
u           = p1(b1:a1,7);    % Wind speed [m/s]
q_a         = p1(b1:a1,9);    % Vapour density of the ambient air [kg/m3]
q_sat       = p1(b1:a1,8);    % Saturation vapour density [kg/m3]
rain        = p1(b1:a1,11);   % Incident precipitation rate [kg/d/m2]
% fuel_moist  = p1(b1:a1,12);   % Fuel Moisture [%]
% fuel_err    = p1(b1:a1,14);   % Fuel Moisture error [%]
m           = p1(b1:a1,13);   % Cloudiness factor [-]
H_p         = p1(b1:a1,10);   % Humidity parameter (hPa/K) [0.001:0.2]

%% Options for the solver %%

abstol = 1e-5; %
reltol = 1e-3; %
o_opts = odeset('AbsTol',abstol,'RelTol',reltol,'Events',@stopevent,'NonNegative',1:4); %

%% Time vector (d) %%

tspan   = linspace(0,length(T_a)-1,length(T_a));
tspan1  = linspace(0,length(T_a)-1,length(T_a));

%% Set Initial Conditions
        
c(1) = T_a(1);                        % Temperature of the outer layer [K]
c(2) = T_a(1);                        % Temperature of the core [K]
c(3) = 0.1*(q(1)*pi*q(2)*q(3)^2);     % Moisture of the outer layer [kg]
c(4) = 0.1*(q(1)*pi*q(2)*q(3)^2);     % Moisture of the core [kg]

%% https://aleksandarhaber.com/simulation-of-ordinary-differential-equations-odes-with-time-varying-coefficients-and-time-varying-inputs-in-matlab/
 try
     warning off 
     tic
     [ty,cu] = ode15s(@(t,x)fuel_model_minutes(t,x,p,q,T_a,L_dn,K_d_diff,K_d_dir,k_d,alp_sha,u,q_a,q_sat,H_p,rain,m,tspan), tspan1, c, o_opts);
     
 catch ME
     warning off
     ty = length(tspan);
     cu = ones(length(tspan),length(c))*1e+99;     
 end
 
 if length(cu) < length(tspan)
    cu = ones(length(tspan),length(c))*1e+99;
end

if isreal(cu)==0
    cu = ones(length(tspan),length(c))*1e+99;    
end

%% Postprocessing %%

T_o     = cu(:,1);  % Temperature of the outer layer [K]
T_c     = cu(:,2);  % Temperature of the core [K]
m_o     = cu(:,3);  % Moisture of the outer layer [kg/s]
m_c     = cu(:,4);  % Moisture of the core [kg/s]
f       = p(1);
we      = 0.045;    % Weight of the stick [kg]
rho_s   = q(1);   % Stick density (400 [kg/m3], Nelson, 2000)
L       = q(2);   % Length of stick [0.41 m]
r       = q(3);   % Radius of stick [0.0065 m]
V_t     = pi*L*r^2;         % Total volume of the stick [m3]
m_s     = (f.*m_o + (1-f).*m_c)*100/(rho_s*V_t); % [fraction]

%% Plotting %%

output = [ty ty m_s T_o T_c m_o m_c T_a rain];

end
