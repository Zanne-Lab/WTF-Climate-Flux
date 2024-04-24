function [output] = calibration_file2_plotting_wood(p)

%% Calibration %%

q(1)  = 0.48*1000;      % Wood log density of pines (352-849 kg/m3) https://matmatch.com/learn/property/density-of-wood
q(2)  = 0.09;           % Length/width of log of wood [0.09 m]
q(3)  = 0.05/2;         % Height/2 of log of wood [0.05 m]
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
[p1,sv] = forcing_minutes_wood(b,p);
q(23)   = sv; 
% fuel    = load("wood_fuel.txt");

if b == 1 % DRO
    b1      = 1;     % 9125 DRO 2237 MLES 2253 MLRF 2513 PNW 6981 STCK 2464 HQ_AWC
    a1      = 35305; % 10505 DRO 19809 MLES 22351 MLRF 10516 PNW STCK 19983 HQ_AWC
%     fuel_moist    = fuel(12:17,1);
%     fuel_err      = fuel(12:17,2);
%     fuel_ind      = fuel(12:17,4);
elseif b == 2   % HQ_AWC
    b1      = 1;     % 28831 DRO 2237 MLES 2253 MLRF 2513 PNW 6981 STCK 2464 HQ_AWC
    a1      = 35305; % 33550 DRO 19809 MLES 22351 MLRF 10516 PNW STCK 19983 HQ_AWC
%     fuel_moist    = fuel(18:22,1);
%     fuel_err      = fuel(18:22,2);
%     fuel_ind      = fuel(18:22,4);
elseif b == 3 % MLES
    b1      = 1;     % 28831 DRO 2237 MLES 2253 MLRF 2513 PNW 6981 STCK 2464 HQ_AWC
    a1      = 35305; % 33550 DRO 19809 MLES 22351 MLRF 10516 PNW STCK 19983 HQ_AWC
%     fuel_moist    = fuel(23:31,1);
%     fuel_err      = fuel(23:31,2);
%     fuel_ind      = fuel(23:31,4);    
elseif b == 4 % MLRF
    b1      = 1;     % 2253 MLRF 2513 PNW 6981 STCK 2464 HQ_AWC ALL 1
    a1      = 35305; % 10551 MLRF 10516 PNW STCK 19983 HQ_AWC ALL 31509
%     fuel_moist    = fuel(32:40,1);
%     fuel_err      = fuel(32:40,2);
%     fuel_ind      = fuel(32:40,4); 
elseif b == 5 % PNW
    b1      = 1;     % 2513 PNW 6981 STCK 2464 HQ_AWC
    a1      = 35305; % 10516 PNW STCK 19983 HQ_AWC
%     fuel_moist    = fuel(41:end,1);
%     fuel_err      = fuel(41:end,2);
%     fuel_ind      = fuel(41:end,4);     
else % STCK
    b1      = 1;     % 2513 PNW 6981 STCK 2464 HQ_AWC
    a1      = 35305; % 10516 PNW STCK 19983 HQ_AWC
%     fuel_moist    = fuel(1:11,1);
%     fuel_err      = fu21263el(1:11,2);
%     fuel_ind      = fuel(1:11,4);      
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

% abstol = 1e-7; %
% reltol = 1e-5; %
abstol = 1e-5; %
reltol = 1e-3; %
o_opts = odeset('AbsTol',abstol,'RelTol',reltol,'Events',@stopevent,'NonNegative',1:4); %

%% Time vector (d) %%

tspan   = linspace(0,length(T_a)-1,length(T_a));
tspan1  = linspace(0,length(T_a)-1,length(T_a));

%% Set Initial Conditions

c(1) = T_a(1);                         % Temperature of the outer layer [K]
c(2) = T_a(1);                       % Temperature of the core [K]
c(3) = 0.1*(q(1)*pi*q(2)*q(3)^2);     % Moisture of the outer layer [kg]
c(4) = 0.1*(q(1)*pi*q(2)*q(3)^2);     % Moisture of the core [kg]

%%

c(1) = 285.692529000105;                        % Temperature of the outer layer [K]
c(2) = 286.118836659950;                        % Temperature of the core [K]
c(3) = 0.0467338808907791;     % Moisture of the outer layer [kg]
c(4) = 0.283536726658756;     % Moisture of the core [kg] (8.5)

% c(1) = 335.931404933427;                        % Temperature of the outer layer [K]
% c(2) = 335.934828674409;                        % Temperature of the core [K]
% c(3) = 0.0514974723840678;     % Moisture of the outer layer [kg]
% c(4) = 0.408158770773818;     % Moisture of the core [kg] (195)

% c(1) = 334.710850558792;                        % Temperature of the outer layer [K]
% c(2) = 334.712573860889;                        % Temperature of the core [K]
% c(3) = 0.0518898256275770;  0.0131699539106538   % Moisture of the outer layer [kg]
% c(4) = 0.406772899638125;     % Moisture of the core [kg] (194)

% c(1) = 330.170998177216;                        % Temperature of the outer layer [K]
% c(2) = 330.172798005812;                        % Temperature of the core [K]
% c(3) = 0.0484498309492642;     % Moisture of the outer layer [kg]
% c(4) = 0.398616443020184;     % Moisture of the core [kg] (190)

% c(1) = 329.988880473936;                        % Temperature of the outer layer [K]
% c(2) = 330.076336528113;                        % Temperature of the core [K]
% c(3) = 0.0464843163921370;     % Moisture of the outer layer [kg]
% c(4) = 0.388848725852860;     % Moisture of the core [kg] (185)

% c(1) = 329.769655734497;                        % Temperature of the outer layer [K]
% c(2) = 329.759015794466;                        % Temperature of the core [K]
% c(3) = 0.0510914223617425;     % Moisture of the outer layer [kg]
% c(4) = 0.391888643585726;     % Moisture of the core [kg] (187)

%% https://aleksandarhaber.com/simulation-of-ordinary-differential-equations-odes-with-time-varying-coefficients-and-time-varying-inputs-in-matlab/
 try
     warning off 
     tic
   [ty,cu] = ode15s(@(t,x)fuel_model_wood(t,x,p,q,T_a,L_dn,K_d_diff,K_d_dir,k_d,alp_sha,u,q_a,q_sat,H_p,rain,m,tspan), tspan1, c, o_opts);
     figure;
     length(cu)  
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
m_o     = cu(:,3);  % Moisture of the outer layer [kg]
m_c     = cu(:,4);  % Moisture of the core [kg]
f       = p(1);
we      = 0.045;    % Weight of the stick [kg]
rho_s   = q(1);     % Stick density (400 [kg/m3], Nelson, 2000)
L       = q(2);     % Length of stick [0.41 m]
r       = q(3);     % Radius of stick [0.0065 m]
V_t     = L*L*r;         % Total volume of the stick [m3]
m_s     = (f.*m_o + (1-f).*m_c)*100/(rho_s*V_t); % [fraction]
T_s     = (f.*T_o + (1-f).*T_c); % [fraction]

%% Plotting %%

output = [ty tspan' m_s T_o T_c m_o m_c T_a rain T_s];

end
