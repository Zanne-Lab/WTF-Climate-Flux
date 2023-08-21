%% PLOTTING RESULTS %%

%% Calling simulation outputs %%

% % load('DRO_sticks_output_2.mat')% accepted
% % load('HQ_AWC_sticks_output_2.mat')% accepted
% % load('MLES_sticks_output_2.mat') % accepted
% % load('MLRF_sticks_output_2.mat') % accepted
% % load('STCK_sticks_output_2.mat') % accepted
% % load('PNW_sticks_output_2.mat') % accepted

%% Calling Dates %%

date  = readtable("DATES.txt","TextType","string"); % 
date  = sortrows(date);

%% Extracting outputs in right format %%

ty          = output(:,1);
tspan       = output(:,2);
m_s         = output(:,3);
T_o         = output(:,4);
T_c         = output(:,5);
m_o         = output(:,6);
m_c         = output(:,7);
fuel_moist  = output(:,8);
T_a         = output(:,9);
rain        = output(:,10);

%% Figure %%

fig = figure;
left_color = [0 0 0];
right_color = [0 0 0];
set(fig,'defaultAxesColorOrder',[left_color; right_color]);

A = plot(date.Var1,fuel_moist,".",'color',[255,27,177]/255,'LineWidth',1);
hold on
B = plot(date.Var1,m_s,'color',[55,126,184]/255,'LineWidth',1);
ylabel('Fuel Moisture (%)')
ylim([0 100])
% yyaxis right
% C = plot(date.Var1,rain*1000,'color',[252,141,98]/255,'LineWidth',1);
ax = gca;
ax.FontSize = 20;
% legend([A B C],'Observations','Simulations','Precipitation','Location','northwest')
legend([A B],'Observations','Simulations','Location','northwest')
% ylabel('Precipitation (mm)')
% ylim([0 50])

