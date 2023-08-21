%% Simulated Annealing %%

% load('cali_STCK_wood.mat')
% load('cali_PNW_wood.mat')
% load('cali_MLES_wood.mat')
% load('cali_HQAWC_wood.mat')
% load('cali_DRO_wood_1.mat')
% load('cali_MLRF_wood.mat')

% p(7)  = 1; 

% ObjectiveFunction = @calibration_file2_minutes;
ObjectiveFunction = @calibration_file2_wood;

% f, A, B, d_s, m_max, ep
% ub = [0.95 20 5 -1 2.5 2.5];
% lb = [0.05 -8 -50 -10 0.1 0.5];

% ub = [0.95 20 5 -1 2.5 p(6) p(7)];
% lb = [0.05 -8 -50 -10 0.1 p(6) p(7)];
% 
% p0 = p;
% 
% options = optimoptions(@simulannealbnd,'MaxFunEvals',100000);
% [p,fval,exitFlag,output] = simulannealbnd(ObjectiveFunction,p0,lb,ub,options);%,options);
 
% save('cali_DRO_wood.mat','p')
% save('cali_STCK_wood.mat','p')
% save('cali_PNW_wood.mat','p')
% save('cali_MLES_wood.mat','p')
% save('cali_MLRF_wood.mat','p')
% save('cali_HQAWC_wood.mat','p')

%%

% load('test_2000f_saB.mat')
% % 
%  ObjectiveFunction = @calibration_file2_minutes;
% % % 
 A = [];
 b = [];
 Aeq = [];
 beq = [];
% f, A, B, d_s, m_max, ep
% ub = [0.95 20 5 -1 1.8 2.5];
% lb = [0.05 -8 -50 -10 0.1 0.5];

% ub = [0.95 20 5 p(4) p(5) p(6) p(7)];
% lb = [0.05 -8 -50 p(4) p(5) p(6) p(7)];

ub = [0.95 p(2) p(3) p(4) 2.5 p(6) 3];
lb = [0.05 p(2) p(3) p(4) 0.1 p(6) 3];

p0 = p;

 nonlcon = [];
 options = optimoptions(@fmincon,'MaxFunEvals',2000);

 [p,fval,exitFlag,output] = fmincon(ObjectiveFunction,p0,A,b,Aeq,beq,lb,ub,nonlcon,options);

% save('cali_DRO_wood_2.mat','p')
% save('cali_STCK_wood_1.mat','p')
% save('cali_PNW_wood_1.mat','p')
% save('cali_MLES_wood_1.mat','p')
% save('cali_MLRF_wood_1.mat','p')
% save('cali_HQAWC_wood_1.mat','p')