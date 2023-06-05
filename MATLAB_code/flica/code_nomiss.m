%% add path 
% NOTE: need to change to your path to the flica folder
addpath 'YOUR PATH\FILICA\MATLAB_code\flica'


%% load dataset

% row: voxels
% col: subjects

clear;

load("data_nomiss.mat")

load("flica_nomiss_info.mat")


%% multimodal image data

clear Y;
Y{1} = mod1_true;
Y{2} = mod2_true;

clear Yc; 
for k = 1:2

    Yc{k} = bsxfun(@minus, Y{k}, mean(Y{k},2)); % demean

    tmprms=rms(Yc{k},2);

    Yc{k} = bsxfun(@rdivide, Yc{k}, tmprms); % scaling 

    Yc{k}(isinf(Yc{k}) | isnan(Yc{k}))=0;

end



%% do LICA

opts.maxits = flica_niter;

opts.num_components = ncomp;tic;tstart = tic;Morig20 = flica(Yc, opts);%Morig20.time = toc(tstart);

%Morig20.icvar=icvar_perc(Yc,Morig20);

clear mod1_true mod1_true_std mod2_true mod2_true_std Y Yc;

save("results_nomiss.mat")




 