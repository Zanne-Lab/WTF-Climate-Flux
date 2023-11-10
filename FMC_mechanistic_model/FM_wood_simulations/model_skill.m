%% Model Skill metrics %5

block_MC = readtable('block_MC.csv');
block_Tp = readtable('block_chambertemp.csv');
block_MC = sortrows(block_MC,"site","ascend");
block_Tp = sortrows(block_Tp,"site","ascend");
%% DRO 
load('cali_DRO_wood_final_output.mat')

% Moisture

DRO_M   = block_MC(1:9,4);
DRO_Ms  = output([9073,13105,18313,21961,26377,30745,35282,7106,9072],3);
RMSE_M  = (sum((DRO_M-DRO_Ms).^2)./length(DRO_Ms))^0.5;
Bias_M  = sum((DRO_Ms-DRO_M))./length(DRO_Ms);


% Temperature

DRO_T   = block_Tp(1:7,4);
DRO_Ts  = output([9073,13105,18313,21961,26377,30745,35282,],10)-273;
RMSE_T  = (sum((DRO_T-DRO_Ts).^2)./length(DRO_Ts))^0.5;
Bias_T  = sum((DRO_Ts-DRO_T))./length(DRO_Ts);

%% MLRF
load('cali_MLRF_wood_final_output.mat')

MLRF_M   = block_MC(20:30,4);
MLRF_Ms  = output([9145,13153,17545,21937,26425,30793,35209,3024,4610,8160,9147],3);
RMSE_M  = (sum((MLRF_M-MLRF_Ms).^2)./length(MLRF_Ms))^0.5;
Bias_M  = sum((MLRF_Ms-MLRF_M))./length(MLRF_Ms);


% Temperature

MLRF_T   = block_Tp(15:21,4);
MLRF_Ts  = output([9145,13153,17545,21937,26425,30793,35209],10)-273;
RMSE_T  = (sum((MLRF_T-MLRF_Ts).^2)./length(MLRF_Ts))^0.5;
Bias_T  = sum((MLRF_Ts-MLRF_T))./length(MLRF_Ts);

%% MLES
load('cali_MLES_wood_final_output.mat')

MLES_M   = block_MC(10:19,4);
MLES_Ms  = output([9145,13153,18193,21937,26425,30793,35209,3025,4608,8161],3);
RMSE_M  = (sum((MLES_M-MLES_Ms).^2)./length(MLES_Ms))^0.5;
Bias_M  = sum((MLES_Ms-MLES_M))./length(MLES_Ms);


% Temperature

MLES_T   = block_Tp(8:14,4);
MLES_Ts  = output([9145,13153,18193,21937,26425,30793,35209],10)-273;
RMSE_T  = (sum((MLES_T-MLES_Ts).^2)./length(MLES_Ts))^0.5;
Bias_T  = sum((MLES_Ts-MLES_T))./length(MLES_Ts);

%% PNW
load('cali_PNW_wood_final_output.mat')

PNW_M   = block_MC(31:39,4);
PNW_Ms  = output([9169,13177,17881,21937,26449,30817,3021,9174,9770],3);
RMSE_M  = (sum((PNW_M-PNW_Ms).^2)./length(PNW_Ms))^0.5;
Bias_M  = sum((PNW_Ms-PNW_M))./length(PNW_Ms);


% Temperature

PNW_T   = block_Tp(22:27,4);
PNW_Ts  = output([9169,13177,17881,21937,26449,30817,],10)-273;
RMSE_T  = (sum((PNW_T-PNW_Ts).^2)./length(PNW_Ts))^0.5;
Bias_T  = sum((PNW_Ts-PNW_T))./length(PNW_Ts);

%% STCK
load('cali_STCK_wood_final_output.mat')

STCK_M   = block_MC(40:51,4);
STCK_Ms  = output([9145,17713,21937,26473,30817,35209,3023,4587,8161,9151,9142,9576],3);
RMSE_M  = (sum((STCK_M-STCK_Ms).^2)./length(STCK_Ms))^0.5;
Bias_M  = sum((STCK_Ms-STCK_M))./length(STCK_Ms);


% Temperature

STCK_T   = block_Tp(28:33,4);
STCK_Ts  = output([9145,17713,21937,26473,30817,35209],10)-273;
RMSE_T  = (sum((STCK_T-STCK_Ts).^2)./length(STCK_Ts))^0.5;
Bias_T  = sum((STCK_Ts-STCK_T))./length(STCK_Ts);
