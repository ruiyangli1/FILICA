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
for k = 1:mod_n
    eval("Yc{k} = mod" + k + "_std;")
end


%% do LICA

opts.maxits = flica_niter;  

opts.num_components = ncomp;opts.initH = H;tic;tstart = tic;Morig20 = flica(Yc, opts);%Morig20.time = toc(tstart);

%Morig20.icvar=icvar_perc(Yc,Morig20);

% clean up objectives
clear H Yc;
for k = 1:mod_n
    eval("clear mod" + k + "_std;")
end
clear k;

% save results
save("results_filica_update.mat")




 