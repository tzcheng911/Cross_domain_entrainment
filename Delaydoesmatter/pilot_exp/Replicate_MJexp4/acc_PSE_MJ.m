
%% accuracy for each of 18 conditions in different difficulty 
clear all
close all
clc
begin_time = [5,8,9];
com_IOI = [1,2,3,4,5,6];
results = zeros(12,3,6);

for sub = 1:19
    str='s0';
    if sub>=10
       str='s';
    end
    data_route=['/Users/tzu-hancheng/Google_Drive/Academia/LASR/Replicate_MJexp4/data/' str sprintf('%d',sub)];
    cd (data_route);
    data = [str num2str(sub) '1test'];
    load (data);
% mark the correct trial as 1 and incorrect trial as 0 in the seventh
% colume in matrix "result"
    for i = 1:size(result,1)
        if (result(i,4) < 4 && result(i,5) == 100) || (result(i,4) > 3 && result(i,5) == 200)
            result(i,7) = 1;
        else 
            result(i,7) = 0;
        end
    end

% calculate accuracy for 108 conditions:
%2(extra tones or not)x3(delay)x3(beginning timing)x6(comparison IOI from shortest to longest) 

            for k = 1:length(begin_time)
                for l = 1:length(com_IOI)
                index = find ((result(:,3)==begin_time(k)) & (result(:,4)==com_IOI(l)));
                corr_index = find(result(index,7)==1);
                results (sub,k,l) = length(corr_index)/length(index);
                end 
            end

end
%% make 5D matrix into 2D
aaa = reshape(results,19,18);

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

clear all
clc

%% Calculate the proportion of judging as shorter
begin_time = [5,8,9];
com_IOI = [1,2,3,4,5,6];
results = zeros(12,3,6);

for sub = 1:19
    str='s0';
    if sub>=10
       str='s';
    end
    data_route=['/Users/tzu-hancheng/Google_Drive/Academia/LASR/Replicate_MJexp4/data/' str sprintf('%d',sub)];
    cd (data_route);
    data = [str num2str(sub) '1test'];
    load (data);

    for i = 1:size(result,1)
        if (result(i,5) == 100) % The trials considered as shorter
            result(i,7) = 1;
        else 
            result(i,7) = 0;
        end
    end

            for k = 1:length(begin_time)
                for l = 1:length(com_IOI)
                index = find ((result(:,3)==begin_time(k)) & (result(:,4)==com_IOI(l)));
                shorter_index = find(result(index,7)==1);
                results (sub,k,l) = length(shorter_index)/length(index);
                end 
            end

end

%% Deal with the 1 and 0 condition
N = 12; % 5 trials for each condition
results_vector = reshape(results,numel(results),1);
ind_1 = find(results_vector == 1);
ind_0 = find(results_vector == 0);
results_vector(ind_1) =  1 - 1/(2*N);
results_vector(ind_0) =  1/(2*N);
results_z = norminv (results_vector);

%% short proportion-z transform
results_norm = reshape (results_z,19,3,6);
mean_results_norm= mean(results_norm,3);

%% Calculate PSE
nM = reshape(results_z,19,3,6); % reshape the original matrix (84 x 18) to new one (6 x 14 x 18) for calculation
PSE = zeros(19,3);
comp_IOI = [550:20:650]';

for i = 1:19
    for j = 1:3
        P = polyfit(comp_IOI,squeeze(nM(i,j,:)),1);
        PSE(i,j) = -P(2)/P(1);
        if PSE(i,j)<0
           PSE(i,j) = nan;
        end
%        if PSE(i,j)<0 || PSE(i,j)>1200
%            PSE(i,j) = nan;
%        end
    end
end
        

