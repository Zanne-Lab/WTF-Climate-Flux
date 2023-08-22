function [p1,sv] = forcing_minutes_cilinder(b,p)

%% Forcing variables %%

if b == 1
    data = load("DRO.txt"); 
    fprintf('DRO \n')
    sv    = 0.101067682; % Sky-view factor [0:1]
elseif b == 2
    data = load("HQ_AWC.txt");
    fprintf('HQ_AWC \n')
    sv    = 0.8; % Sky-view factor [0:1]
elseif b == 3
    data = load("MLES.txt"); 
    fprintf('MLES \n')
    sv    = 0.194019734; % Sky-view factor [0:1]
elseif b == 4
    data = load("MLRF.txt"); 
    fprintf('MLRF \n')
    sv    = 0.089480551; % Sky-view factor [0:1]
elseif b == 5
    data = load("PNW.txt");
    fprintf('PNW \n')
    sv    = 0.904595439; % Sky-view factor [0:1]
else
    data = load("STCK.txt");    
    fprintf('STCK \n')
    sv    = 0.650740587; % Sky-view factor [0:1]
end

%% Calling raw data %%

fuel_moisture   = data(:,3);        % Fuel moisture (%)
air_temp        = data(:,8);        % Averaged air temperature (°C)
precip          = data(:,5);        % Precipitation (mm)
press           = data(:,6);        % Mean air pressure (mb)
rel_hum         = data(:,7);        % Relative humidity (%)
wind            = data(:,9);        % Mean daily wind speed (m/h)
wind_height     = 2;                % Wind speed height observation (m)
ele_angle       = data(:,13)*pi/180; % Solar elevation angle (radians)
azi_angle       = data(:,14)*pi/180;% Solar azimuth angle (radians)
k_d             = data(:,10);       % Solar incoming radiation (W/m2) [direct+diffuse]
L_dn            = data(:,12);       % Downwelling longwave radiation (W/m2)

%% Processing the raw data %%

% h               = 2.00;         % Distance between ground and stick (cm)
K_solar         = 1367;         % Solar constant (W/m2)
tau             = 0.75;         % Atmospheric transmissivity
p_sea           = 1013.25;      % Pressure at the site and at sea-level (mb)
phi             = ele_angle;    % Solar elevation angle (radians)
pr              = press;        % Mean air pressure (mb)
K_d_max         = K_solar.*tau.^(pr./p_sea).*sin(phi); % Theoretical maximum solar radiation achievable 
n               = k_d./K_d_max; % Clearness index "n"
r               = 0.035;        % Height of log of wood [0.05 m]
L               = 0.010;        % Length/width of log of wood]
psi             = azi_angle;    % Solar azimuth angle (radians)
z1              = 2*r;          % Height of the stick (m)
zh              = (2*r)*(3/4);  % Zero plane displacement (m) 
zo              = 0.004;        % Surface roughness (m)**
z2              = wind_height;  % Height where the wind speed is measured (m)
M               = 0.018;        % Molecular mass of water [kg mol-1]
R               = 8.314e-3;     % Gas constant [m^3 kPa K-1 mol-1]
ep              = p(6);         % Empirical coefficien

%% Averaged air temperature [K] : 

T_a           = air_temp + 273; 
p1(1,:)       = T_a;

%% Downwelling longwave radiation (J/h/m2)

p1(2,:)           = L_dn*60*60; 

%% Cloudiness factor :

m = zeros(length(n),1); 

for i=1:length(n)
if n(i) > 0.22
    m(i)       = 1 - n(i);                               
else
    m(i)       = 1;
end
end

for i=1:length(n)
    if m(i) < 0
        m(i) = 0;
    else
        m(i) = m(i);
    end
end
% 
p1(13,:)           = m; 

%% Diffuse short wave radiation [J/m2/(h)] :

K_d_diff = zeros(length(n),1); 

for i=1:length(n)
    
    if n(i) <= 0.22
        K_d_diff(i) = k_d(i)*(1-0.09*n(i));
    elseif n(i) > 0.8
        K_d_diff(i) = k_d(i)*(0.165);
    else
        K_d_diff(i) = k_d(i)*(0.951 - 0.1604*n(i) + 4.388*n(i)^2 - 16.638*n(i)^3 + 12.336*n(i)^4);
    end
    
end

for i=1:length(K_d_diff)
    if K_d_diff(i) > 0
        K_d_diff(i) = K_d_diff(i);
    else
        K_d_diff(i) = 0;
    end
end

p1(3,:)           = sv*K_d_diff*60*60; 

%% Direct short wave radiation [J/m2/(h)] :

K_d_dir     = k_d - K_d_diff; % [J/d/m2]
tao_dir     = exp((-ep.*phi.*cos(phi)).*(exp(-(sv-0.45)/0.29)).*(sin(phi)).^(-1));

for i=1:length(tao_dir)
    if tao_dir(i) > 0
        tao_dir(i) = tao_dir(i);
    else
        tao_dir(i) = 0;
    end
end

p1(4,:)     = tao_dir.*K_d_dir*60*60;        % 
p1(5,:)     = tao_dir.*K_d_dir*60*60 + sv*K_d_diff*60*60;            %   

%% Area of the shadow cast by the stick on a horizontal plane [m2]

alp_sha     = 2*r*L.*csc(phi).*((1-((cos(phi)).^2).*((cos(psi)).^2)).^0.5) + pi*r^2.*cot(phi).*cos(phi);  
for i=1:length(alp_sha)
    if alp_sha(i) > 0
        alp_sha(i)     = alp_sha(i); 
    else
        alp_sha(i)     = 1;
    end
end

alp_sha(find(alp_sha==Inf)) = 1;

p1(6,:)     = alp_sha; 

%% Wind speed [m/d]

u           = wind.*log((z1-zh)/zo).*(log((z2-zh)./zo)).^(-1); %  Wind speed regression calculation
p1(7,:)     = u;

%% Saturation vapour density in [kg/m3]

e_0         = 0.61094.*exp(17.625.*air_temp./(air_temp+243.04)); % Saturation vapor pressure (kPa) [Koutsoyiannis, D. (2012)]
q_sat       = e_0*M./(R*T_a); % Saturation vapour density (kg/m3)
p1(8,:)     = q_sat; % Saturation vapour density (kg/m3)
q_a         = q_sat.*rel_hum/100; % Vapour density of the ambient air (kg/m3)
p1(9,:)     = q_a;   % Vapour density of the ambient air (kg/m3)

%% Humidity parameter [hPa/K]

H_p         = e_0*10./T_a; % Humidity parameter [hPa/K]
p1(10,:)    = H_p;         % Humidity parameter [hPa/K]

%% Incident precipitation rate [kg/(30 min)/m2]

p1(11,:) = precip/1000;

%% Normalization 

% a = 80;
% b = 5;
% A = max(fuel_moisture);
% B = min(fuel_moisture);
% 
% fuel_moisture_1 = zeros(length(fuel_moisture),1);
% 
% for i = 1:length(fuel_moisture)
%     fuel_moisture_1(i) = b + (a - b)*(fuel_moisture(i) - B)/(A-B);
% end
%% Fuel moisture data [%]

% fuel_err = zeros(length(fuel_moisture),1);
% 
% for i = 1:length(fuel_moisture)
%     if fuel_moisture_1(i) < 10
%         fuel_err(i) = 1.0;
%     elseif  10 <= fuel_moisture_1(i) && fuel_moisture_1(i) < 20
%         fuel_err(i) = 1.5;
%     elseif  20 <= fuel_moisture_1(i) && fuel_moisture_1(i) < 30
%         fuel_err(i) = 2.2;
%     elseif  30 <= fuel_moisture_1(i) && fuel_moisture_1(i) < 50
%         fuel_err(i) = 3.0;
%     else
%         fuel_err(i) = 5.0;
%     end
% end

% p1(12,:) = fuel_moisture_1;
% p1(14,:) = fuel_err;
p1 = p1'; % 

end