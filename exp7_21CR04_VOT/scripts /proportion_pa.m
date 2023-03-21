
clear all
clc

%% Calculate the proportion of judging as shorter
begin_time = [1:3];
VOT = [1:6];
nsub = 17;
results = zeros(nsub,6,3);

for sub = 1:nsub
    str='s0';
    if sub>=10
       str='s';
    end
    data_route=['/Users/tzu-hancheng/Google_Drive/Academia/2nd_project/data/' str sprintf('%d',sub)];
    cd (data_route);
    data = [str num2str(sub) '2test'];
    load (data);

    for i = 1:size(result,1)
        if (result(i,3) == 200) % The trials considered as /pa/
            result(i,6) = 1;
        else 
            result(i,6) = 0;
        end
    end
            for l = 1:length(VOT)
                for k = 1:length(begin_time)
                index = find ((result(:,1)==VOT(l)) & (result(:,2)==begin_time(k)));
                pa_index = find(result(index,6)==1);
                results (nsub,l,k) = length(pa_index)/length(index);
                end 
            end
end
%%
VOT = [8.845,15.279,21.039,26.821,31.357,36.731,42.128,47.162];
numTrials = 12;
for plot_VOT = 1:length(VOT)
    ind = find((result(:,1) == plot_VOT) & (result(:,3) == 200)); % percent of reporting /pa/
    prop(plot_VOT) = length(ind)/numTrials;
end
x = [1:6];
[est,p] = fit_logistic(VOT,prop);
figure;
plot(x,prop,'o')  % data
hold on
plot(x,est,'LineWidth',2) 

% prop = Qinf./(1 + exp(-alpha*(x_est-thalf)));
% prop = p(2)./(1 + exp(-p(3)*(x_est-p(1))));
x_est = -(log(p(2)/0.5-1))/p(3)+p(1);
perceptual_boundary = x_est;
