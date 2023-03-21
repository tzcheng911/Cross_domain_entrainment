%%
% The important things are the IOI, W_phi and W_p. With these three things,
% we can calculate the phi(i), P(i), then the C = phi(i) + IOI(i)/P(i).
% PSE can be calculated if we know C, if C < 0, shorter, if C > 0, longer.
% So the proportion of responding short will be 1 in some comparion
% intervals (i.e. with the pre-defined weight in early condition: 550, 570, 590, 610),
% while 0 in some comparion (i.e. 630, 650). Again, this is influenced by the
% W_phi and W_p and delay length IOIs (early, on-time, late)
% One fun fact is that if you change the delay length from 1.2 to 1200 or 
% whatever you want, you will still get the same outputs. So the delay
% length is not matter in their model. 

%% Early, On-time, Late condition
i = 5;
comp = [0.55 0.57 0.59 0.61 0.63 0.65];
IOI = [0.6 1.2 comp(i)]; % i = 1-6
W_phi = 0.5; % 0.8
W_p = 0.05; % 0.05
[phi,P,C] = timekeeperMJ(IOI, W_phi, W_p);
early = -0.18;
late = 0.18;
IOI_early = [0.6 1.2+early comp(i)]; % i = 1-6
IOI_late = [0.6 1.2+late comp(i)]; % i = 1-6
[phi,P,C] = timekeeperMJ(IOI_early, W_phi, W_p);

%% Prediction for each comp interval
clear 
comp = [0.55 0.57 0.59 0.61 0.63 0.65];
W_phi = 1; % 0.8
W_p = 0.1; % 0.05
early = -0.18;
late = 0.18;

for i = 1:length(comp)
IOI = [0.6 1.2 comp(i)]; % i = 1-6
IOI_early = [0.6 1.2+early comp(i)]; % i = 1-6
IOI_late = [0.6 1.2+late comp(i)]; % i = 1-6
[phi,P,C_ontime] = timekeeperMJ(IOI, W_phi, W_p);
[phi,P,C_early] = timekeeperMJ(IOI_early, W_phi, W_p);
[phi,P,C_late] = timekeeperMJ(IOI_late, W_phi, W_p);
C_ontime_all(i) = C_ontime(1,3);
C_early_all(i) = C_early(1,3);
C_late_all(i) = C_late(1,3);
end

%% Plot the final C with the change of the parameters IOI = 600
clear 
clc
close all
i = 5;
W_p = 0.05; % 0.05

range = [0:0.01:1];
comp = [0.55 0.57 0.59 0.61 0.63 0.65];
early = -0.18;
late = 0.18;
IOI = [0.6 1.2 comp(i)]; % i = 1-6
IOI_early = [0.6 1.2+early comp(i)]; % i = 1-6
IOI_late = [0.6 1.2+late comp(i)]; % i = 1-6

for np = 1:length(range)
    W_phi = range(np);
    [phi,P,C] = timekeeperMJ(IOI_early, W_phi, W_p);
    C_array(np) = C(1,3);
end
plot(range,C_array,'.')
title('Comparions duration 5')
xlabel('W phi')
ylabel('Final contrast (negative - short, positive - long)')

%% Plot the final C with the change of the parameters IOI = 300 -- doesn't change the result from IOI = 600 ms
clear 
clc
close all
i = 5;
W_p = 0.1; % 0.05

range = [0:0.01:1];
comp = [0.275 0.285 0.295 0.305 0.315 0.325];
early = -0.09;
late = 0.09;
IOI = [0.3 0.6 comp(i)]; % i = 1-6
IOI_early = [0.3 0.6+early comp(i)]; % i = 1-6
IOI_late = [0.3 0.6+late comp(i)]; % i = 1-6

for np = 1:length(range)
    W_phi = range(np);
    [phi,P,C] = timekeeperMJ(IOI_early, W_phi, W_p);
    C_array(np) = C(1,3);
end
plot(range,C_array,'.')
title('Comparions duration 5')
xlabel('W phi')
ylabel('Final contrast (negative - short, positive - long)')

%% Proportion short with changing phi and P parameters
addpath('/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp1/exp1_script/scripts')
clear 
clc

comp = [0.55 0.57 0.59 0.61 0.63 0.65];
early = -0.18;
late = 0.18;
range = [0:0.01:1];
for np = 1:length(range)
    for nphi = 1:length(range)
        W_phi = range(nphi);
        W_p = range(np);
        for ncomp = 1:length(comp)
            IOI = [0.6 1.2+late comp(ncomp)]; 
            [phi,P,tempC] = timekeeperMJ(IOI, W_phi, W_p);
            C(ncomp) = tempC(end);
            neg = length(find(C < 0)); % respond short 
            pps(np,nphi) = neg/6;
        end
    end
end
figure;
imagesc(pps)
axis xy
ticklabels = range(1:10:end);
ticks = linspace(1,length(range),numel(ticklabels));
set(gca, 'XTick', ticks, 'XTickLabel', ticklabels,'YTick', ticks, 'YTickLabel', ticklabels,'FontSize',14)
xlabel('W phi')
ylabel('W Period')
c = colorbar;
colormap(parula(7));
unique(pps)