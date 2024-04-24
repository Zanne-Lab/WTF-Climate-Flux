%% fmincom %%

ObjectiveFunction = @calibration_file2_minutes;

 A = [];
 b = [];
 Aeq = [];
 beq = [];
% f, A, B, d_s, m_max, ep
ub = [0.95 20 5 -1 1.8 2.5 p(7)];
lb = [0.05 -8 -50 -10 0.1 0.5 p(7)];

p0 = p;

nonlcon = [];
options = optimoptions(@fmincon,'MaxFunEvals',10000);

[p,fval,exitFlag,output] = fmincon(ObjectiveFunction,p0,A,b,Aeq,beq,lb,ub,nonlcon,options);

save('cali_<<site>>_stick_output_corrected','output')
