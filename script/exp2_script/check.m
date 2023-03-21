%% check if the files in data_analayis identical to the data_raw
cd('/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp1/data_analysis')
raw_file = ' s08 1test.mat';
output_file = 's082.mat';
raw = load(raw_file);
output = load(output_file);
isequal(raw.result,output.result)

%% check if the mean of the acc, pps and PSE are the same as the excel
sublist = [3,4,6:8,10,12:19,21:23,25:32,34,36:37,39:41,47:63];
% EXP1 Accuracy
results = results(sublist,:,:);
mean_acc = squeeze(mean(results,1));

% EXP1 PPS
results = results(sublist,:,:);
mean_pps = squeeze(mean(results,1));

% EXP1 PSE
results = PSE(sublist,:);
mean_PSE = squeeze(mean(results,1));
clear sublist

% EXP2 Accuracy
results = results(sub_list,:,:,:);
mean_acc = squeeze(mean(results,1));
reshape(mean_acc(1,:,:),numel(mean_acc(1,:,:)),1);
reshape(mean_acc(2,:,:),numel(mean_acc(2,:,:)),1);
reshape(mean_acc(3,:,:),numel(mean_acc(3,:,:)),1);

% EXP2 PPS
results = results(sub_list,:,:,:);
mean_pps = squeeze(mean(results,1));
reshape_mean_pps = reshape(mean_pps, numel(mean_pps),1);
reshape(mean_pps(1,:,:),numel(mean_pps(1,:,:)),1);
reshape(mean_pps(2,:,:),numel(mean_pps(2,:,:)),1);
reshape(mean_pps(3,:,:),numel(mean_pps(3,:,:)),1);

% EXP2 PSE
results = PSE(sub_list,:,:);
mean_PSE = squeeze(mean(results,1));
reshape_mean_PSE = reshape(mean_PSE, numel(mean_PSE),1);
reshape(mean_PSE(1,:),numel(mean_PSE(1,:)),1);
reshape(mean_PSE(2,:),numel(mean_PSE(2,:)),1);
reshape(mean_PSE(3,:),numel(mean_PSE(3,:)),1);

%% check if the data fed in R
% EXP1
squeeze(mean(mean(results,1),3)); % interaction effects
squeeze(mean(PSE,1)); 

% EXP2
squeeze(mean(mean(results,1),4)); % interaction effects
squeeze(mean(mean(mean(results,1),2),4)); % main effect of onset
squeeze(mean(mean(mean(results,1),3),4)); % main effect of onset

PSE = PSE([1:21,23:48],:,:);
squeeze(mean(mean(PSE,1),4)); % interaction effects
squeeze(mean(mean(mean(PSE,1),2),4)); % main effect of onset
squeeze(mean(mean(mean(PSE,1),3),4)); % main effect of delay