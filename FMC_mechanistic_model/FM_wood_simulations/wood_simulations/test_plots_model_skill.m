%% PLOTTING RESULTS %%

%% Calling simulation outputs %%

% % load('DRO_wood_output_7.mat')% old
% % load('HQ_AWC_wood_output_7.mat')% old
% % load('PNW_wood_output_7.mat')% old
% % load('MLES_wood_output_7.mat')% old
% % load('MLRF_wood_output_7.mat')% old
% % load('STCK_wood_output_7.mat')% old


% % load('HQ_AWC_wood_output_7_A.mat')% accepted
% % load('PNW_wood_output_7_A.mat')% accepted
% % load('STCK_wood_output_7_A.mat')% accepted
% % load('MLES_wood_output_7_A.mat')% accepted
% % load('MLRF_wood_output_7_A.mat')% accepted
% % load('DRO_wood_output_7_A.mat')% accepted


%% Calling Dates for simulations %%

date1   = readtable("DATES.txt","TextType","string"); % 
date1   = sortrows(date1);

% Extracting outputs in right format %%

ty          = output(:,1);
tspan       = output(:,2);
m_s         = output(:,3);
T_o         = output(:,4);
T_c         = output(:,5);
m_o         = output(:,6);
m_c         = output(:,7);
T_a         = output(:,8);
rain        = output(:,9);
T_s        = output(:,10);

%% DRO %%

% fuel        = load("wood_fuel.txt");
% fuel_moist  = fuel(12:17,1);
date        = readtable("DRO_wood.txt","TextType","string"); % 
date        = sortrows(date);
fuel        = load("DRO_wood_FM.txt");
fuel_mean   = fuel(:,2);

fig = figure;
left_color = [0 0 0];
right_color = [0 0 0];
set(fig,'defaultAxesColorOrder',[left_color; right_color]);
% A = plot(date.Var1,fuel_moist,"*",'MarkerSize',10,'color',[255,27,177]/255,'LineWidth',1);
A = plot(fuel(:,1),fuel_mean,"*",'MarkerSize',10,'color',[255,27,177]/255,'LineWidth',1);
hold on
% B = plot(date1.Var1,m_s,'MarkerSize',10,'color',[55,126,184]/255,'LineWidth',1);
B = plot(tspan,m_s,'MarkerSize',10,'color',[55,126,184]/255,'LineWidth',1);
ylabel('Fuel Moisture (%)')
ylim([0 300])
yyaxis right
C = plot(tspan,rain,'color',[252,141,98]/255,'LineWidth',1);
ax = gca;
ax.FontSize = 20;
legend([A B],'Observations','Simulations','Location','northwest')
% legend([A B C],'Observations','Simulations','Precipitation','Location','northwest')
% ylabel('Precipitation (mm)')
% ylim([0 50])

%% MLES %%

% fuel        = load("wood_fuel.txt");
% fuel_moist  = fuel(23:31,1);
% date        = readtable("MLES_wood.txt","TextType","string"); % 
% date        = sortrows(date);
fuel        = load("MLES_wood_FM.txt");
fuel_mean   = fuel(:,2);

fig = figure;
left_color = [0 0 0];
right_color = [0 0 0];
set(fig,'defaultAxesColorOrder',[left_color; right_color]);
% A = plot(date.Var1,fuel_moist,"*",'MarkerSize',10,'color',[255,27,177]/255,'LineWidth',1);
A = plot(fuel(:,1),fuel_mean,"*",'MarkerSize',10,'color',[255,27,177]/255,'LineWidth',1);
hold on
% B = plot(date1.Var1,m_s,".",'MarkerSize',10,'color',[55,126,184]/255,'LineWidth',1);
B = plot(tspan,m_s,'MarkerSize',10,'color',[55,126,184]/255,'LineWidth',1);
ylabel('Fuel Moisture (%)')
ylim([0 300])
yyaxis right
C = plot(tspan,rain,'color',[252,141,98]/255,'LineWidth',1);
ax = gca;
ax.FontSize = 20;
legend([A B],'Observations','Simulations','Location','northwest')
% legend([A B C],'Observations','Simulations','Precipitation','Location','northwest')
% ylabel('Precipitation (mm)')
% ylim([0 50])

%% MLRF %%

% fuel        = load("wood_fuel.txt");
% fuel_moist  = fuel(32:40,1);
% date        = readtable("MLRF_wood.txt","TextType","string"); % 
% date        = sortrows(date);
% tspan       = linspace(1,length(T_a)-1,length(T_a));
fuel        = load("MLRF_wood_FM.txt");
fuel_mean   = fuel(:,2);

fig = figure;
left_color = [0 0 0];
right_color = [0 0 0];
set(fig,'defaultAxesColorOrder',[left_color; right_color]);
% A = plot(date.Var1,fuel_moist,"*",'MarkerSize',10,'color',[255,27,177]/255,'LineWidth',1);
A = plot(fuel(:,1),fuel_mean,"*",'MarkerSize',10,'color',[255,27,177]/255,'LineWidth',1);
hold on
% B = plot(date1.Var1,m_s,".",'MarkerSize',10,'color',[55,126,184]/255,'LineWidth',1);
B = plot(tspan,m_s,'MarkerSize',10,'color',[55,126,184]/255,'LineWidth',1);
ylabel('Fuel Moisture (%)')
ylim([0 300])
yyaxis right
C = plot(tspan,rain,'color',[252,141,98]/255,'LineWidth',1);
ax = gca;
ax.FontSize = 20;
legend([A B],'Observations','Simulations','Location','northwest')
% legend([A B C],'Observations','Simulations','Precipitation','Location','northwest')
% ylabel('Precipitation (mm)')
% ylim([0 50])

%% PNW %%
%% 

% fuel        = load("wood_fuel.txt");
% fuel_moist  = fuel(41:end,1);
% date        = readtable("PNW_wood.txt","TextType","string"); % 
% date        = sortrows(date);
fuel        = load("PNW_wood_FM.txt");
fuel_mean   = fuel(:,2);

fig = figure;
left_color = [0 0 0];
right_color = [0 0 0];
set(fig,'defaultAxesColorOrder',[left_color; right_color]);
% A = plot(date.Var1,fuel_moist,"*",'MarkerSize',10,'color',[255,27,177]/255,'LineWidth',1);
A = plot(fuel(:,1),fuel_mean,"*",'MarkerSize',10,'color',[255,27,177]/255,'LineWidth',1);
hold on
% B = plot(date1.Var1,m_s,'MarkerSize',10,'color',[55,126,184]/255,'LineWidth',1);
B = plot(tspan,m_s,'MarkerSize',10,'color',[55,126,184]/255,'LineWidth',1);
ylabel('Fuel Moisture (%)')
ylim([0 300])
yyaxis right
C = plot(tspan,rain,'color',[252,141,98]/255,'LineWidth',1);
ax = gca;
ax.FontSize = 20;
legend([A B],'Observations','Simulations','Location','northwest')
% legend([A B C],'Observations','Simulations','Precipitation','Location','northwest')
% ylabel('Precipitation (mm)')
% ylim([0 50])

%% STCK %%

% fuel        = load("wood_fuel.txt");
% fuel_moist  = fuel(1:11,1);
% date        = readtable("STCK_wood.txt","TextType","string"); % 
% date        = sortrows(date);
fuel        = load("STCK_wood_FM.txt");
fuel_mean   = fuel(:,2);

fig = figure;
left_color = [0 0 0];
right_color = [0 0 0];
set(fig,'defaultAxesColorOrder',[left_color; right_color]);
% A = plot(date.Var1,fuel_moist,"*",'MarkerSize',10,'color',[255,27,177]/255,'LineWidth',1);
A = plot(fuel(:,1),fuel_mean,"*",'MarkerSize',10,'color',[255,27,177]/255,'LineWidth',1);
hold on
% B = plot(date1.Var1,m_s,".",'MarkerSize',10,'color',[55,126,184]/255,'LineWidth',1);
B = plot(tspan,m_s,'MarkerSize',10,'color',[55,126,184]/255,'LineWidth',1);
ylabel('Fuel Moisture (%)')
ylim([0 300])
yyaxis right
C = plot(tspan,rain,'color',[252,141,98]/255,'LineWidth',1);
ax = gca;
ax.FontSize = 20;
legend([A B],'Observations','Simulations','Location','northwest')
% legend([A B C],'Observations','Simulations','Precipitation','Location','northwest')
% ylabel('Precipitation (mm)')
% ylim([0 50])

%% Mean value plot %%

fig = figure;
xlim([0 length(tspan)]);
ylim([0 250])
yline(mean(fuel_mean),'color',[55,126,184]/255,'LineWidth',2)
hold on
yyaxis right
plot(tspan,rain,'color',[252,141,98]/255,'LineWidth',1);
ax = gca;
ax.FontSize = 20;

%% NSE %%

DRO_simulated   = m_s([fuel([2:9],1)]);
DRO_mean        = mean(fuel_mean([2:9]));
DRO_nse         = 1 - abs(sum((DRO_simulated - fuel_mean([2:9])).^2))/...
                  abs(sum((fuel_mean([2:9])-DRO_mean).^2))
DRO_nse1        = 1 - abs(sum((DRO_simulated - fuel_mean([2:9]))))/...
                  abs(sum((fuel_mean([2:9])-DRO_mean)));
DRO_bias       = sum(DRO_simulated - fuel_mean([2:9]))/length(DRO_simulated)
DRO_rmse       = ((sum((DRO_simulated - fuel_mean([2:9])).^2))/length(DRO_simulated)).^0.5
scatter(fuel_mean([2:9]),DRO_simulated)

%%

MLES_simulated  = m_s([fuel(:,1)]);
MLES_mean       = mean(fuel_mean);
MLES_nse        = 1 - abs(sum((MLES_simulated - fuel_mean).^2))/...
                  abs(sum((fuel_mean-MLES_mean).^2))
MLES_bias       = sum(MLES_simulated - fuel_mean)/length(MLES_simulated)
MLES_rmse       = ((sum((MLES_simulated - fuel_mean).^2))/length(MLES_simulated)).^0.5
scatter(fuel_mean,MLES_simulated)

%%

MLRF_simulated  = m_s([fuel(:,1)]);
MLRF_mean       = mean(fuel_mean);
MLRF_nse        = 1 - abs(sum((MLRF_simulated - fuel_mean).^2))/...
                  abs(sum((fuel_mean-MLRF_mean).^2))
MLRF_bias       = sum(MLRF_simulated - fuel_mean)/length(MLRF_simulated)
MLRF_rmse       = ((sum((MLRF_simulated - fuel_mean).^2))/length(MLRF_simulated)).^0.5
scatter(fuel_mean,MLRF_simulated)

%%
v=10;
PNW_simulated   = m_s([fuel(1:v,1)]);
PNW_mean        = mean(fuel_mean(1:v,1));
PNW_nse         = 1 - (sum(abs(PNW_simulated - fuel_mean(1:v,1)).^2))/...
                  (sum(abs(fuel_mean(1:v,1)-PNW_mean).^2))
PNW_bias       = sum(PNW_simulated - fuel_mean)/length(PNW_simulated)
PNW_rmse       = ((sum((PNW_simulated - fuel_mean).^2))/length(PNW_simulated)).^0.5
scatter(fuel_mean,PNW_simulated)

%%

STCK_simulated  = m_s([fuel(:,1)]);
STCK_mean       = mean(fuel_mean);
STCK_nse        = 1 - (sum(abs(STCK_simulated - fuel_mean).^2))/...
                  (sum(abs(fuel_mean-STCK_mean).^2))
STCK_bias       = sum(STCK_simulated - fuel_mean)/length(STCK_simulated)
STCK_rmse       = ((sum((STCK_simulated - fuel_mean).^2))/length(STCK_simulated)).^0.5
scatter(fuel_mean,STCK_simulated)
%%
