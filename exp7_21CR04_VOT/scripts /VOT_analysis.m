clear all
close all
clc

%% Check perceptual boundary in the identification task
nsub = 16;
for sub = 1:nsub
    str='s0';
    if sub>=10
       str='s';
    end
    data_route=['/Users/tzu-hancheng/Google_Drive/Research/entrainedVOT/real2019/data/' str sprintf('%d',sub)];
    cd (data_route);
    data = [str num2str(sub) '1test'];
    load (data);
    PB(sub) = perceptual_boundary;
    all_lb(sub) = lb;
    all_hb(sub) = hb;
end
steps = [all_lb;all_hb];
PB = PB';
%% Calculate the proportion of judging as pa
addpath('/Applications/fit_logistic')
begin_time = [1:3];
nsub = 16;
results = zeros(nsub,3,6);

for sub = 1:nsub
    str='s0';
    if sub>=10
       str='s';
    end
    data_route=['/Users/tzu-hancheng/Google_Drive/Research/entrainedVOT/real2019/data/' str sprintf('%d',sub)];
    cd (data_route);
    data = [str num2str(sub) '2test'];
    load (data);
if sub>=18 
    KB = 100; % 200: F for /ba/, J for /pa/ i.e. s01 - s17; 100: J for /ba/, F for /pa/ i.e. s18 - s29
else KB = 200;
end

%VOT = [8.845,15.279,21.039,26.821,31.357,36.731,42.128,47.162];
VOT = [1:6];
% switch file_VOTs{1} % get the 6 VOTs used by the participants
%     case 'VOT_1.wav'
%          VOT = VOT(1:6);
%     case 'VOT_2.wav'
%          VOT = VOT(2:7);
%     case 'VOT_3.wav' 
%          VOT = VOT(3:8);
% end

numTrials = 18;
for bt = 1:length(begin_time)
    for v = 1:length(VOT)
    ind = find((result(:,1) == v) & (result(:,2)==begin_time(bt)) & (result(:,4) == KB)); % percent of reporting /pa/
    results (sub,bt,v) = length(ind)/numTrials;
    end
    [est,p] = fit_logistic(VOT',squeeze(results(sub,bt,:)));
    lg_est(sub,bt,:) = est;
    lg_p(sub,bt,:) = p;
    half = mean([max(est) min(est)]);
%    perceptual_boundary(sub,bt) = -(log(lg_p(sub,bt,2))/half-1)/lg_p(sub,bt,3)+lg_p(sub,bt,1);
end
perceptual_boundary = squeeze(lg_p(:,:,1)); % inflection point
given_VOT(sub,:) = file_VOTs;

end
% figure;
% plot(squeeze(results(sub,bt,:)),'o')  % data
% hold on
% plot(est','LineWidth',2) 
early = squeeze(results(:,1,:));
ontime = squeeze(results(:,2,:));
late = squeeze(results(:,3,:));
%% plot grand average
mean_real = squeeze(mean(results,1));
mean_est = squeeze(mean(lg_est,1));
figure;
plot(mean_real','o')  % real data for early, ontime, late
hold on
plot(mean_est','LineWidth',2)  % est data for early, ontime, late
legend('Early','Ontime','Late')

%% plot individual data points with log fitting
x = 1:6;
%for n = 1:3
for n = 1:size(results,1)
    figure
    plot(x,squeeze(lg_est(n,1,:)),'r',x,squeeze(lg_est(n,2,:)),'b',x,squeeze(lg_est(n,3,:)),'k','LineWidth',2)
    hold on 
    plot(x,squeeze(results(n,1,:)),'ro',x,squeeze(results(n,2,:)),'bo',x,squeeze(results(n,3,:)),'ko','LineWidth',2)
    legend('Early','On-Time','Late','Location','southeast')
    str='s0';
    if n>=10
       str='s';
    end
    name = [str num2str(n)];
    title(name)
    saveas(gcf,[name '.jpg']);
end

%%
x = VOT(1:6);
x1 = VOT(1):0.1:VOT(6);
for n = 1:3
%for n = 1:size(results,1)
    y1 = squeeze(lg_p(n,1,3))./(1+exp(-1*(x1-squeeze(lg_p(n,1,1)))));
    y2 = squeeze(lg_p(n,2,3))./(1+exp(-1*(x1-squeeze(lg_p(n,2,1)))));
    y3 = squeeze(lg_p(n,3,3))./(1+exp(-1*(x1-squeeze(lg_p(n,3,1)))));
    figure
    plot(x1,y1,'r',x1,y2,'b',x1,y3,'k','LineWidth',2)
    hold on 
    plot(x,squeeze(results(n,1,:)),'ro',x,squeeze(results(n,2,:)),'bo',x,squeeze(results(n,3,:)),'ko','LineWidth',2)
    legend('Early','On-Time','Late','Location','southeast')
    str='s0';
    if n>=10
       str='s';
    end
    name = [str num2str(n)];
    title(name)
%    saveas(gcf,[name '.jpg']);
end
%% Logistic function
L = p(3);
k = 1;
x0 = p(1);
x = 1:0.1:6;
y = L./(1+exp(-k*(x-x0)));
figure
plot(x,y,'LineWidth',2);

%% identification task - extract the perceptual boundary of the threshold test

