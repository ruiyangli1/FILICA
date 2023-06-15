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
for k = 1:mod_n
    eval("Yc{k} = mod" + k + "_std_cmplt;")
end


%% do LICA

opts.maxits = flica_niter;

opts.num_components = ncomp;tic;tstart = tic;Morig20 = flica(Yc, opts);%Morig20.time = toc(tstart);

%Morig20.icvar=icvar_perc(Yc,Morig20);

% clean up objectives
clear Yc;
for k = 1:mod_n
    eval("clear mod" + k + "_std_cmplt;")
end
clear k;

% save results
save("results_completer.mat")




 