%% add path 
% NOTE: need to change to your path to the flica folder
addpath 'YOUR PATH\FILICA\MATLAB_code\flica'


%% load dataset

% row: voxels
% col: subjects

clear;

load("data_std_update.mat")

load("filica_step2_info.mat")


%% multimodal image data

clear Yc;
Yc{1} = mod1_std;
Yc{2} = mod2_std;


%% do LICA

opts.maxits = flica_niter;  

opts.num_components = ncomp;opts.initH = H;tic;tstart = tic;Morig20 = flica(Yc, opts);%Morig20.time = toc(tstart);

%Morig20.icvar=icvar_perc(Yc,Morig20);

clear Yc mod1_std mod2_std H;

save("results_filica_update.mat")





 