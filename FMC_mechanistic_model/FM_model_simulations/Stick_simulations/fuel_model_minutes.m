
function vf_ = fuel_model_minutes(t,x,p,q,T_a,L_dn,K_d_diff,K_d_dir,k_d,alp_sha,u,q_a,q_sat,H_p,rain,m,tspan)

%% State variables

T_o     = x(1); % Temperature of the outer layer [K]
T_c     = x(2); % Temperature of the core [K]
m_o     = x(3); % Moisture of the outer layer [kg]
m_c     = x(4); % Moisture of the core [kg]

%% Model Parameters

f       = p(1); % Fraction of the stick volume taken up by the outer layer. [0.05:0.90]
A       = p(2); % Empirical constant
B       = p(3); % Empirical constant
d_s     = 10^p(4); % Bulk diffusion coefficient of the stick [m2/(h)]
m_max   = p(5); % Maximum value of moisture content of the outer layer

%% Model Fix Parameters

rho_s       = q(1);   % Stick density (400 [kg/m3], Nelson, 2000), wood density (352-849)
L           = q(2);   % Length of stick [0.41 m]
r           = q(3);   % Radius of stick [0.0065 m]
eps_s       = q(4);   % Stick emissivity [0.85]
sig         = q(5);   % Stephan-Boltzmann constant [J/(h)/m2/K4]
eps_g       = q(6);   % Emissivity of the ground [0.95]
eps_v       = q(7);   % Emissivity of the vegetation [0.965]
a_1         = q(8);   % Fit parameter 1 [1.2]
a_2         = q(9);   % Fit parameter 2 [3.0]
a_3         = q(10);  % Fit exponent 3  [0.5]
Ce          = q(11);  % Climatological value [46.5 cm K/hPa]
beta        = q(12);  % Constant based on cloud type [0.26]
alp_s       = q(13);  % Stick Albedo [0.65]
alp_g       = q(14);  % Ground albedo [0.185]
rho_a       = q(15);  % Density of air [1.093 kg/m3]
c_a         = q(16);  % Specific heat of air [1005 J/kg/K]
k           = q(17);  % Thermal diffusivity of the air [m2/h]
v           = q(18);  % Kinematic viscosity of air [m2/h]
M           = q(19);  % Molecular mass of water [0.018 kg mol-1]
R           = q(20);  % Gas constant [8.314e-3 m^3 kPa K-1 mol-1]
g           = q(21);  % Specific gravity of the stick [0.42]
c_water     = q(22);  % Specific heat of water [4200 J/K/kg]
s           = q(23);  % Sky-view factor [0:1] 0.5**
cv          = q(24);  % Vegetation contribution coefficient [0:1] 0.5**
dv          = q(25);  % Density of water [1000 kg/m3]

%% Forcing variables

T_a         = interp1(tspan,T_a,t);         % Air Temperature [K]
L_dn        = interp1(tspan,L_dn,t);        % Downwelling longwave radiation (J/(h)/m2)
K_d_diff    = interp1(tspan,K_d_diff,t);    % Diffuse shor twave radiation [J/(h)/m2]
K_d_dir     = interp1(tspan,K_d_dir,t);     % Direct short wave radiation [J/(h)/m2]
k_d         = interp1(tspan,k_d,t);         % Total short wave radiation [J/(h)/m2]
alp_sha     = interp1(tspan,alp_sha,t);     % Area of the shadow cast by the stick on a horizontal plane [m2]
u           = interp1(tspan,u,t);           % Wind speed [m/s]
q_a         = interp1(tspan,q_a,t);         % Vapour density of the ambient air [kg/m3]
q_sat       = interp1(tspan,q_sat,t);       % Saturation vapour density [kg/m3]
H_p         = interp1(tspan,H_p,t);         % Humidity parameter (hPa/K) [0.001:0.2]
rain        = interp1(tspan,rain,t);        % Incident precipitation rate [kg/(h)/m2]
m           = interp1(tspan,m,t);           % Cloudiness factor [-]

%% Forcing variables and extra functions

% Dimension of the stick
V_t   = pi*L*r^2;         % Total volume of the stick [m3] 
r_c   = r*(1 - f)^0.5;    % Radius of the core [m]
V_c   = pi*L*r_c^2;       % Volume of the core [m3] 
V_o   = V_t - V_c;        % Volume of the outer layer [m3] 
a_o   = 2*pi*r*L;         % Surface area of the stick [m2]
a_s   = a_o + 2*pi*r^2;   % Surface area of the entire stick [m2] 
m_o_1 = m_o/(rho_s*V_o);  % Moisture of the outer layer [fraction]
m_c_1 = m_c/(rho_s*V_c);  % Moisture of the core layer [fraction]

% Longwave incoming radiation [J/(h)]
w       = Ce.*H_p;                                      % Precipitable water content [cm]
eps_sky = 1 - (1+w).*exp(-(a_1+a_2.*w).^(a_3));         % Clear sky emissivity
eps_a   = (1 + beta.*m).*eps_sky;                       % Atmospheric emissivity
eps_cp  = (1-cv)*eps_g + cv*eps_v;                      % Canopy emissivity
L_d     = (s.*eps_a + (1-s)*eps_cp).*L_dn;              % Downwelling longwave radiation [J/(h)/m2]
% L_d     = L_dn;                                           % Downwelling longwave radiation (J/(h)/m2)
L_u     = eps_g*sig.*T_a.^4;                              % Upwelling longwave [J/(h)/m2]
L_abs   = (pi*L*r + 2*pi*r^2).*eps_s.*(L_d + L_u);        % Absorbed longwave radiation [J/(h)]

% Shortwave diffuse radiation [J/(h)]
K_abs_diff  = (pi*L*r + 2*pi*r^2).*(1-alp_s).*(K_d_diff + k_d.*alp_g); 

% Shortwave direct radiation [J/(h)] 
K_abs_dir   = alp_sha.*(1-alp_s).*K_d_dir; % Direct solar radiation absorbed by the stick [J/(h)]

% Longwave emitted radiation [J/(h)/m2]
L_emit  = eps_s*sig*T_o^4; % Emitted longwave radiation 

% Sensible heat flux Sensible [J/(h)/m2]
R_e     = u.*2.*r./v;                              % Reynolds Number
N_u     = 0.17.*R_e.^0.62;                         % Nusselt number
omega   = 2.*r.*(k.*N_u).^(-1);                    % Aerodynamic resistance [(h)/m]
Q_h     = rho_a.*c_a.*(T_o-T_a).*omega.^(-1);      % Sensible heat flux [J/(h)/m2]

% Water vapor and latent heat flux [kg/m2/(h)] 
RH_surf = exp(-4.19.*M.*(exp(m_o_1.*B + A)).*(R.*T_o).^(-1));
q_surf = q_sat.*RH_surf;
E = (q_surf - q_a).*(omega.^(-1)); % Vapour mass flux [kg/m2/(h)]

% Latent energy flux [J/(h)/m2]
m_s         = f.*m_o_1 + (1-f).*m_c_1; % average stick moisture [fraction]
lambda_vap  = 2.501e6 - 2.37e3.*(T_a-273.15); % Latent heat of vaporization Stull 1988, page 279 [J/kg]
lambda_sorp = 21000.*exp(-14.*m_s).*M.^(-1);  % Differential heat of sorption [J/kg]
Q_e         = (lambda_vap + lambda_sorp).*E;  % Latent energy flux [J/(h)/m2]

% Conduction [J/(h)]
ro_mid  = (r_c + r)/2; % Mid-point radius of the outer layer
rc_mid  = r_c/2; % Mid-point radius of the core
k_s     = (g.*(0.1941 + 0.004064.*m_s*100) + 0.01864)*3600; % Bulk conductivity of the stick [J/m/(h)/K]
C       = 2*pi*L*k_s*(T_o-T_c).*(log(ro_mid/rc_mid)).^(-1); % Flux of heat between the two layers

% Stick specific heat calculation c_s  [J/K/kg]
T_s     = f.*T_o + (1 - f).*T_c; % Average stick temperature [K]
c_wood  = 103.1 + 3.867.*T_s;   % Specific heat of dry wood [J/K/kg]
c_bound = (23.55 * T_s - 1320*m_s - 6191)*m_s; % Energy absorbed by the bound water below the fiber saturation point [J/K/kg]
c_s     = (c_wood + m_s.*c_water).*(1+m_s).^(-1) + c_bound; 

% Diffusion [kg/(h)]
D = 2*pi*L*d_s*rho_s.*(m_o_1-m_c_1)./(log(ro_mid/rc_mid)); % Rate of diffusion into the core from the outer layer [kg/h]

% Precipitation [kg/(h)]

% P_inc = min(rain.*r.*L,m_max.*rho_s.*V_o-max(0,m_o+(-a_o.*E-D)));  % Absorbed precipitation [kg/h]
P_inc = min(rain.*r.*L.*dv,m_max.*rho_s.*V_o-max(0,m_o));  % Absorbed precipitation [kg/h]

% P_abs = max(0,P_inc); % Absorbed precipitation [kg/h]
P_abs = P_inc; % Absorbed precipitation [kg/h]

%% ODE system 

vf_ = zeros(4,1);

% Temperature of the outer layer [K]
vf_(1) = (L_abs + K_abs_diff + K_abs_dir - a_s.*L_emit - a_s.*Q_h - ...
         (a_o).*Q_e - C).*(c_s.*rho_s*V_o).^(-1); 
%          (a_s - 2*pi*r^2).*Q_e - C).*(c_s.*rho_s*V_o).^(-1); 
     
% Temperature of the core [K]
vf_(2) = C.*(c_s.*rho_s.*V_c).^(-1);

% Moisture of the outer layer [kg]
% vf_(3) = P_abs - (a_s - 2*pi*r^2).*E - D;
vf_(3) = P_abs - (a_o).*E - D;

% Moisture of the core [kg]
vf_(4) = D; 

end
