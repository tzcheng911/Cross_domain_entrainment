clear all
clc

%% Calculate the proportion of judging as pa
begin_time = [1:3];
nsub = 17;
results = zeros(nsub,3,6);

for sub = 1:nsub
    str='s0';
    if sub>=10
       str='s';
    end
    data_route=['/Users/tzu-hancheng/Google_Drive/Academia/2nd_project/data/' str sprintf('%d',sub)];
    cd (data_route);
    data = [str num2str(sub) '2test'];
    load (data);

VOT = [8.845,15.279,21.039,26.821,31.357,36.731,42.128,47.162];

switch file_VOTs{1} % get the 6 VOTs used by the participants
    case 'VOT_1.wav'
         VOT = VOT(1:6);
    case 'VOT_2.wav'
         VOT = VOT(2:7);
    case 'VOT_3.wav' 
         VOT = VOT(3:8);
end

numTrials = 12;
for bt = 1:length(begin_time)
    for v = 1:length(VOT)
    ind = find((result(:,1) == v) & (result(:,2)==begin_time(bt)) & (result(:,3) == 200)); % percent of reporting /pa/
    results (sub,bt,v) = length(ind)/12;
    end
    [est,p] = fit_logistic(VOT,squeeze(results(sub,bt,:)));
    lg_est(sub,bt,:) = est;
    lg_p(sub,bt,:) = p;
    half = mean([max(est) min(est)]);
    perceptual_boundary(sub,bt) = -(log(lg_p(sub,bt,2))/half-1)/lg_p(sub,bt,3)+lg_p(sub,bt,1);
end
given_VOT(sub,:) = file_VOTs;

end
% figure;
% plot(squeeze(results(sub,bt,:)),'o')  % data
% hold on
% plot(est','LineWidth',2) 
%% plot
mean_real = squeeze(mean(results,1));
mean_est = squeeze(mean(lg_est,1));
figure(1);
plot(mean_real','LineWidth',2)  % real data for early, ontime, late
legend('Early','Ontime','Late')
figure(2);
plot(mean_est','LineWidth',2)  % est data for early, ontime, late
legend('Early','Ontime','Late')

%% identification task

