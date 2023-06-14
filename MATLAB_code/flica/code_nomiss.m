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
for k = 1:mod_n
    eval("Y{k} = mod" + k + "_true;")
end

% standardization
clear Yc; 
for k = 1:mod_n

    Yc{k} = bsxfun(@minus, Y{k}, mean(Y{k},2)); % demean

    tmprms=rms(Yc{k},2);

    Yc{k} = bsxfun(@rdivide, Yc{k}, tmprms); % scaling 

    Yc{k}(isinf(Yc{k}) | isnan(Yc{k}))=0;

end



%% do LICA

opts.maxits = flica_niter;

opts.num_components = ncomp;tic;tstart = tic;Morig20 = flica(Yc, opts);%Morig20.time = toc(tstart);

%Morig20.icvar=icvar_perc(Yc,Morig20);


% clean up objectives
clear Y Yc;
for k = 1:mod_n
    eval("clear mod" + k + "_true;")
end
clear k;
clear tmprms; 


% save results
save("results_nomiss.mat")




 