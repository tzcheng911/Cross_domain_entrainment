clear 
close all
clc

sublist = [3,4,6:8,10,12:19,21:23,25:32,34,36:37,39:41,47:63];
%% accuracy for each of 18 conditions in different difficulty 
begin_time = [5,8,9];
com_IOI = [1,2,3,4,5,6];
results = zeros(63,3,6);

for sub = 1:63
    str = 's0';
    cond = '4'; %or '4'
    if sub >= 10
       str = 's';
    end
    data_route=['/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp1/data_analysis'];
    cd (data_route);
    data = [str num2str(sub) cond];
    load (data);
    if result(1,2) ~= str2num(cond) || size(result,1) ~= 216 
       error('Wrong file.'); 
    end
% mark the correct trial as 1 and incorrect trial as 0 in the eighth
% colume in matrix "result"
    for i = 1:size(result,1)
        if (result(i,4) < 4 && result(i,5) == 100) || (result(i,4) > 3 && result(i,5) == 200)
            result(i,8) = 1;
        else 
            result(i,8) = 0;
        end
    end

            for k = 1:length(begin_time)
                for l = 1:length(com_IOI)
                index = find ((result(:,3)==begin_time(k)) & (result(:,4)==com_IOI(l)));
                corr_index = find(result(index,8)==1);
                results (sub,k,l) = length(corr_index)/length(index);
                end 
            end
end
accuracy = reshape(results,63,18);

%% Block analysis (4 blocks) of accuracy to see the learning effect
bk = [{1:54},{55:108},{109:162},{163:216}];
for sub = 1:63
    str = 's0';
    cond = '4'; %or '4'
    if sub >= 10
       str = 's';
    end
    data_route=['/Users/tzu-hancheng/Google_Drive/Academia/LASR/model_distinguish_task_delay/data_analysis'];
    cd (data_route);
    data = [str num2str(sub) cond];
    load (data);
    if result(1,2) ~= str2num(cond) || size(result,1) ~= 216 
       error('Wrong file.'); 
    end
% mark the correct trial as 1 and incorrect trial as 0 in the eighth
% colume in matrix "result"
    for i = 1:size(result,1)
        if (result(i,4) < 4 && result(i,5) == 100) || (result(i,4) > 3 && result(i,5) == 200)
            result(i,8) = 1;
        else 
            result(i,8) = 0;
        end
    end
for k = 1:4
                corr_index = find(result(bk{k},8)==1);
                results (sub,k) = length(corr_index)/length(bk{1});
end
end

accuracy = results;

%% Calculate PSE for all conditions
% The matrix of proportion of judging short 
% 14 subs x 6 comparison IOIs x 18 conditions (84 x 18)
% I: Standard IOIs 1, 6 IOI; D: Delay duration 2,3,4 IOI; B: Beginning modulation early(-0.18s),ontime(0s),late(0.18s)
% s01       I1D1B1	I2D1B1	I1D2B1	I2D2B1	I1D3B1	I2D3B1	I1D1B2	I2D1B2	I1D2B2	I2D2B2	I1D3B2	I2D3B2	I1D1B3	I2D1B3	I1D2B3	I2D2B3	I1D3B3	I2D3B3
%      550
%      570
%      590
%      610
%      630
%      650
% s02       I1D1B1	I2D1B1	I1D2B1	I2D2B1	I1D3B1	I2D3B1	I1D1B2	I2D1B2	I1D2B2	I2D2B2	I1D3B2	I2D3B2	I1D1B3	I2D1B3	I1D2B3	I2D2B3	I1D3B3	I2D3B3
%      550
%      570
%      590
%      610
%      630
%      650

clear 
clc

%% Calculate the proportion of judging as shorter
begin_time = [5,8,9];
com_IOI = [1,2,3,4,5,6];
results = zeros(63,3,6);

for sub = 1:63
    str='s0';
    cond = '2'; %or '4'
    if sub>=10
       str='s';
    end
    data_route=['/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp1/data_analysis'];
    cd (data_route);
    data = [str num2str(sub) cond];
    load (data);

    for i = 1:size(result,1)
        if (result(i,5) == 100) % The trials considered as shorter
            result(i,8) = 1;
        else 
            result(i,8) = 0;
        end
    end

            for k = 1:length(begin_time)
                for l = 1:length(com_IOI)
                index = find ((result(:,3)==begin_time(k)) & (result(:,4)==com_IOI(l)));
                shorter_index = find(result(index,8)==1);
                results (sub,k,l) = length(shorter_index)/length(index);
                end 
            end

end

%% Deal with the 1 and 0 condition
N = 12; % 12 trials for each condition
results_vector = reshape(results,numel(results),1);
ind_1 = find(results_vector == 1);
ind_0 = find(results_vector == 0);
results_vector(ind_1) =  1 - 1/(2*N);
results_vector(ind_0) =  1/(2*N);
results_z = norminv (results_vector);

% short proportion-z transform
results_norm = reshape (results_z,63,3,6);
mean_results_norm= mean(results_norm,3);

% Calculate PSE
nM = reshape(results_z,63,3,6); % reshape the original matrix (84 x 18) to new one (6 x 14 x 18) for calculation
PSE = zeros(63,3);
comp_IOI = [550:20:650]';

for i = 1:63
    for j = 1:3
        y = squeeze(nM(i,j,:));
        [P,S] = polyfit(comp_IOI,y,1);
        PSE(i,j) = -P(2)/P(1); % P is a row vector of length N+1 containing the polynomial coefficients in
                               % descending powers, P(1)*X^N + P(2)*X^(N-1) +...+ P(N)*X + P(N+1).
        R2(i,j) = 1 - (S.normr/norm(y - mean(y)))^2; % calculate R squared: 1 - (unexplained VAR/Total VAR)
        clear y P S
    end
end
        
%% model prediction
ps = 1;
interval = [ps ps ps 0 0 0; ps ps ps 0 0 0; ps ps ps 0 0 0];
entrainment = [ps ps ps ps 0 0; ps ps ps 0 0 0; ps ps 0 0 0 0];
N = 100; 
ind_1 = find(interval == 1);
ind_0 = find(interval == 0);
interval(ind_1) =  1 - 1/(2*N);
interval(ind_0) =  1/(2*N);

ind_1 = find(entrainment == 1);
ind_0 = find(entrainment == 0);
entrainment(ind_1) =  1 - 1/(2*N);
entrainment(ind_0) =  1/(2*N);

interval_z = norminv (interval);
entrainment_z = norminv (entrainment);

comp_IOI = [550:20:650];
for j = 1:3
        P = polyfit(comp_IOI,interval_z(j,:),1);
        PSE(j) = -P(2)/P(1);
        if PSE(j)<0
           PSE(j) = nan;
        end
end
