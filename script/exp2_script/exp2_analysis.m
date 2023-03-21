%% Quick check of the two easiest accuracy
com_IOI = [1,2,3,4,5,6];

    for i = 1:size(result,1)
        if (result(i,4) < 4 && result(i,5) == 100) || (result(i,4) > 3 && result(i,5) == 200)
            result(i,8) = 1;
        else 
            result(i,8) = 0;
        end
    end
    for comp = 1:length(com_IOI)
        index = find ((result(:,4)==com_IOI(comp)));
        corr_index = find(result(index,8)==1);
        accuracy (comp) = length(corr_index)/length(index); % sub, delay length (short, medium, long), onset time, comp
    end 
0.5*(accuracy(1)+accuracy(6))

%% file path
clear 
close all
clc

% data
data_path = '/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp2/tidy_data_first48/';
new_path = '/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp2/data_analysis/';

% outliers data
% data_path = '/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp2/outliers/';
% new_path = '/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp2/data_analysis_outliers/';

cd (data_path);
sub_list = 1:48;
%sub_list = [1,3:13,15:17,19:21,23:42,43:48];
%sub_list = [5,9,11,17,22];
nsub = length(sub_list);

%% Load, rename and save the data to sx2, sx4, sx8
counterbalance = [2,4,8;4,2,8;4,8,2;2,8,4;8,2,4;8,4,2];
counterbalance = repmat(counterbalance,[8,1]);
for sub = sub_list
    for bk = 1:3
    str = 's0';
    if sub >= 10
       str = 's';
    end
    cd(strcat(data_path,str,num2str(sub)))
    data = [str num2str(sub) num2str(bk) 'test.mat'];
    load (data);
    new_data = [new_path str num2str(sub) num2str(result(1,2)) 'test'];    
    save(new_data,'result', 'subDetails', 'timerecord')
    clear result subDetails timerecord
    end
end

%% ***Dangerous*** 
% Correct the 100 for short and 200 for long 
% for sub = sub_list
%     for bk = 1:3
%     str = 's0';
%     if sub >= 10
%        str = 's';
%     end
%     cd(strcat(data_path,str,num2str(sub)))
%     data = [str num2str(sub) num2str(bk) 'test'];
%     load (data);
%     ind100  = find(result(:,5)==100);
%     ind200  = find(result(:,5)==200);
%     result(ind100,5) = 200;
%     result(ind200,5) = 100;
%     clear ind100 ind200
%     save(data,'result', 'subDetails', 'timerecord')
%     end
% end

%% Accuracy - store in results(sub,bk,onset,comp)
begin_time = [5,8,9];
delay = [2,4,8];
com_IOI = [1,2,3,4,5,6];
nsub = length(sub_list);
cd(new_path)
for sub = sub_list
    for bk = 1:length(delay)
        str = 's0';
        if sub >= 10
            str = 's';
        end
        data = [str num2str(sub) num2str(delay(bk)) 'test'];
        load (data);
        for i = 1:size(result,1)
            if (result(i,4) < 4 && result(i,5) == 100) || (result(i,4) > 3 && result(i,5) == 200)
                result(i,8) = 1;
            else 
                result(i,8) = 0;
            end
        end

            for onset = 1:length(begin_time)
                for comp = 1:length(com_IOI)
                index = find ((result(:,3)==begin_time(onset)) & (result(:,4)==com_IOI(comp)));
                corr_index = find(result(index,8)==1);
                results (sub,bk,onset,comp) = length(corr_index)/length(index); % sub, delay length (short, medium, long), onset time, comp
                end 
            end
    end
end

%%
csvwrite('acc.csv',results)

%% Accuracy check (stop rule = 0.6): easiest comparison duration in short, median and long delay
%% Outliers based on the accuracy < 0.6: 2, 14, 18, 22, 29, 33, 41, 42, 43 2020/2/25 Zoe
%% Outliers because other reasons (i.e. can't finish the task, can't pass the practice): 3, 7, 11, 24 2020/2/25 Zoe
acc_short = squeeze(mean(results(sub_list,1,:,[1,6]),3));
acc_medium = squeeze(mean(results(sub_list,2,:,[1,6]),3));
acc_long = squeeze(mean(results(sub_list,3,:,[1,6]),3));

%% Accuracy for short, medium and long delay
acc_short_all = squeeze(mean(mean(results(sub_list,1,:,:),3),4));
acc_medium_all = squeeze(mean(mean(results(sub_list,2,:,:),3),4));
acc_long_all = squeeze(mean(mean(results(sub_list,3,:,:),3),4));
acc_delay_all = [acc_short_all,acc_medium_all,acc_long_all];

%% Accuracy for early, ontime, late
acc_early_all = squeeze(mean(mean(results(sub_list,:,1,:),2),4));
acc_ontime_all = squeeze(mean(mean(results(sub_list,:,2,:),2),4));
acc_late_all = squeeze(mean(mean(results(sub_list,:,3,:),2),4));
acc_onset_all = [acc_early_all,acc_ontime_all,acc_late_all];

%% Proportion short results (sub,bk,onset,comp)
begin_time = [5,8,9];
delay = [2,4,8];
com_IOI = [1,2,3,4,5,6];
nsub = length(sub_list);
cd(new_path)

for sub = sub_list
    for bk = 1:length(delay)
        str = 's0';
        if sub >= 10
            str = 's';
        end
        data = [str num2str(sub) num2str(delay(bk)) 'test'];
        load (data);

    for i = 1:size(result,1)
        if (result(i,5) == 100) % The trials considered as shorter
            result(i,8) = 1;
        else 
            result(i,8) = 0;
        end
    end

            for onset = 1:length(begin_time)
                for comp = 1:length(com_IOI)
                index = find ((result(:,3)==begin_time(onset)) & (result(:,4)==com_IOI(comp)));
                pps_index = find(result(index,8)==1);
                results (sub,bk,onset,comp) = length(pps_index)/length(index);
                end 
            end
    end
end

%%
csvwrite('pss.csv',results)

%% Proportion: pool the onset time together
pps_short = squeeze(mean(results(sub_list,1,:,:),3));
pps_medium = squeeze(mean(results(sub_list,2,:,:),3));
pps_long = squeeze(mean(results(sub_list,3,:,:),3));
pps_short_all = squeeze(mean(mean(results(sub_list,1,:,:),3),4));
pps_medium_all = squeeze(mean(mean(results(sub_list,2,:,:),3),4));
pps_long_all = squeeze(mean(mean(results(sub_list,3,:,:),3),4));
pps_delay_all = [pps_short_all,pps_medium_all,pps_long_all];

%% Proportion short for early, ontime, late
pps_early_all = squeeze(mean(mean(results(sub_list,:,1,:),2),4));
pps_ontime_all = squeeze(mean(mean(results(sub_list,:,2,:),2),4));
pps_late_all = squeeze(mean(mean(results(sub_list,:,3,:),2),4));
pps_onset_all = [pps_early_all,pps_ontime_all,pps_late_all];
mean(pps_onset_all,1)

%% Proportion short for early, ontime, late in short, medium and long delay
pps_short = squeeze(mean(results(sub_list,1,:,:),4));
pps_medium = squeeze(mean(results(sub_list,2,:,:),4));
pps_long = squeeze(mean(results(sub_list,3,:,:),4));
mean(pps_short,1)
mean(pps_medium,1)
mean(pps_long,1)

%% PSE - store in PSE(sub,bk,onset)
% Deal with the 1 and 0 condition
N = 12; % 12 trials for each condition
results_vector = reshape(results,numel(results),1);
ind_1 = find(results_vector == 1);
ind_0 = find(results_vector == 0);
results_vector(ind_1) =  1 - 1/(2*N);
results_vector(ind_0) =  1/(2*N);
results_z = norminv (results_vector);

% short proportion-z transform
results_norm = reshape (results_z,nsub,3,3,6);
mean_results_norm= mean(results_norm,4);

% Calculate PSE
nM = reshape(results_z,nsub,3,3,6); % reshape the original matrix (84 x 18) to new one (6 x 14 x 18) for calculation
PSE = zeros(nsub,3,3);
comp_IOI = [275:10:325]';

for i = 1:nsub
    for j = 1:3
        for k = 1:3
            y = squeeze(nM(i,j,k,:));
            [P,S] = polyfit(comp_IOI,y,1);
            PSE(i,j,k) = -P(2)/P(1);
            R2(i,j,k) = 1 - (S.normr/norm(y - mean(y)))^2; % calculate R squared: 1 - (unexplained VAR/Total VAR)
            clear y P S
        end
    end
end

%%
csvwrite('PSE.csv',PSE)

%% PSE main effect
PSE_onset = squeeze(mean(PSE,3));
PSE_delay = squeeze(mean(PSE,2));

%% PSE interaction effect
PSE_short_onset = squeeze(PSE(:,1,:));
PSE_medium_onset = squeeze(PSE(:,2,:));
PSE_long_onset = squeeze(PSE(:,3,:));
mean(PSE_short_onset,1)
mean(PSE_medium_onset,1)
mean(PSE_long_onset,1)

%% Check for outliers 
% PSE
PSE_short_outlier = isoutlier(PSE_short_onset, 'mean');
PSE_medium_outlier = isoutlier(PSE_medium_onset, 'mean');
PSE_long_outlier = isoutlier(PSE_long_onset, 'mean');

% accuracy
a = find(acc_short(:,1)<0.6);
b = find(acc_short(:,2)<0.6);
c = find(acc_medium(:,1)<0.6);
d = find(acc_medium(:,2)<0.6);
e = find(acc_long(:,1)<0.6);
f = find(acc_long(:,2)<0.6);
unique([a;b;c;d;e;f]); % check if all of the outliers failed at least one block

