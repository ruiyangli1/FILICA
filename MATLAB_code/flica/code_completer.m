%% add path 
% NOTE: need to change to your path to the flica folder
addpath 'YOUR PATH\FILICA\MATLAB_code\flica'


%% load dataset

% row: voxels
% col: subjects

clear;

load("data_std.mat")

load("flica_completer_info.mat")


%% multimodal image data

clear Yc;
Yc{1} = mod1_std_cmplt;
Yc{2} = mod2_std_cmplt;


%% do LICA

opts.maxits = flica_niter;

opts.num_components = ncomp;tic;tstart = tic;Morig20 = flica(Yc, opts);%Morig20.time = toc(tstart);

%Morig20.icvar=icvar_perc(Yc,Morig20);

clear mod1_std mod1_std_cmplt mod2_std mod2_std_cmplt Yc;

save("results_completer.mat")




 