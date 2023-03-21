clear 
close all


%% RT for each subj
RT = zeros(216,63);
outlier = [1,2,5,9,11,20,24,33,35,38,42,43,44,45,46];

for sub = 1:63
    str = 's0';
    cond = '2'; %or '4'
    if sub >= 10
       str = 's';
    end
    data_route=['/Users/tzu-hancheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp1/data_analysis'];
    cd (data_route);
    data = [str num2str(sub) cond];
    load (data);
    rawRT(:,sub) = result(:,6);
    outliermap(:,sub) = ~isoutlier(result(:,6),'mean'); % find the outliers
end

RT = rawRT.*outliermap; 
mean_RT = sum(RT,1)./sum(RT~=0,1); 
grandmean_RT = mean(nonzeros(RT),1); 