%% for practice
clear all
close all
clc

fs = 44100; %sampling rate
dur = 0.06; %duration
f0 = 440;
sIOI = 0.60; %Standard IOI
cIOI = [0.5, 0.7];% Comparison IOI: short(0.55, 0.57, 0.59), long(0.61, 0.63, 0.65)
beginning = [-0.18, 0 , 0.18]; % Beginning modulation: early(-0.18s),ontime(0s),late(0.18s)
delay = [sIOI+beginning;2*sIOI+beginning;3*sIOI+beginning]; % Delay duration: 2,3,4 IOI

nt = 0:1/fs:dur-1/fs;
N = length(nt);
Y_unramp = cos(2*pi*f0*nt);
rampDur = floor(0.005 *fs) - 1;
ramp_f = linspace(0, 1, rampDur);
ramp_b = linspace(1, 0, rampDur);
window = [ramp_f ones(1, N-2*rampDur) ramp_b];
Y = Y_unramp.* window;

standard_tr = [Y zeros(1,round(fs*(sIOI-dur)))]; % the whole IOI
int1_standard = repmat(standard_tr,1,2);
int6_standard = repmat(standard_tr,1,7);
comparison_1 = [Y zeros(1,round(fs*(cIOI(1)-dur)))];
comparison1 = repmat(comparison_1,1,2);
comparison_2 = [Y zeros(1,round(fs*(cIOI(2)-dur)))];
comparison2 = repmat(comparison_2,1,2);

std_int = {int1_standard;int6_standard};
comp_int = {comparison1,comparison2};

std_IOI = {'1','6'};
delay_dur = {'2','3','4'}; 
begin_time = {'8'}; % correspond to early(1), ontime(2), late(3) beginning
comp_IOI = {'1','2'}; % correspond to easy short 0.5(1) and easy long 0.7(2) cIOI

for i = 1:length(std_IOI)
    for j = 1:length(delay_dur)
        for k = 1:length(begin_time)
            for l = 1:length(comp_IOI)
            filename = ['practice_int' std_IOI{i} '_delay' delay_dur{j} '_' begin_time{k} comp_IOI{l} '.wav'];
            sound_file = [std_int{i} zeros(1,round(fs*delay(j,k))) comp_int{l}];
            audiowrite(filename,sound_file,fs);
            end
        end
    end
end
clear all
close all
clc

fs = 44100; %sampling rate
dur = 0.06; %duration
f0 = 440;
sIOI = 0.60; %Standard IOI
cIOI = [0.55, 0.57, 0.59, 0.61, 0.63, 0.65];% Comparison IOI: short(0.55, 0.57, 0.59), long(0.61, 0.63, 0.65)
beginning = [-0.18, 0 , 0.18]; % Beginning modulation: early(-0.18s),ontime(0s),late(0.18s)
delay = [sIOI+beginning;2*sIOI+beginning;3*sIOI+beginning]; % Delay duration: 2,3,4 IOI

nt = 0:1/fs:dur-1/fs;
N = length(nt);
Y_unramp = cos(2*pi*f0*nt);
rampDur = floor(0.005 *fs) - 1;
ramp_f = linspace(0, 1, rampDur);
ramp_b = linspace(1, 0, rampDur);
window = [ramp_f ones(1, N-2*rampDur) ramp_b];
Y = Y_unramp.* window;

standard_tr = [Y zeros(1,round(fs*(sIOI-dur)))]; % the whole IOI
int1_standard = repmat(standard_tr,1,2);
int6_standard = repmat(standard_tr,1,7);
comparison_1 = [Y zeros(1,round(fs*(cIOI(1)-dur)))];
comparison1 = repmat(comparison_1,1,2);
comparison_2 = [Y zeros(1,round(fs*(cIOI(2)-dur)))];
comparison2 = repmat(comparison_2,1,2);
comparison_3 = [Y zeros(1,round(fs*(cIOI(3)-dur)))];
comparison3 = repmat(comparison_3,1,2);
comparison_4 = [Y zeros(1,round(fs*(cIOI(4)-dur)))];
comparison4 = repmat(comparison_4,1,2);
comparison_5 = [Y zeros(1,round(fs*(cIOI(5)-dur)))];
comparison5 = repmat(comparison_5,1,2);
comparison_6 = [Y zeros(1,round(fs*(cIOI(6)-dur)))];
comparison6 = repmat(comparison_6,1,2);

std_int = {int1_standard;int6_standard};
comp_int = {comparison1,comparison2,comparison3,comparison4,comparison5,comparison6};

std_IOI = {'1','6'};
delay_dur = {'2','3','4'}; 
begin_time = {'5','8','9'}; % correspond to early(1), ontime(2), late(3) beginning
comp_IOI = {'1','2','3','4','5','6'}; % correspond to 0.55(1), 0.57(2), 0.59(3), 0.61(4), 0.63(5), 0.65(6) cIOI

for i = 1:length(std_IOI)
    for j = 1:length(delay_dur)
        for k = 1:length(begin_time)
            for l = 1:length(comp_IOI)
            filename = ['int' std_IOI{i} '_delay' delay_dur{j} '_' begin_time{k} comp_IOI{l} '.wav'];
            sound_file = [std_int{i} zeros(1,round(fs*delay(j,k))) comp_int{l}];
            audiowrite(filename,sound_file,fs);
            end
        end
    end
end

%% for main task
clear all
close all
clc

fs = 44100; %sampling rate
dur = 0.06; %duration
f0 = 440;
sIOI = 0.60; %Standard IOI
cIOI = [0.55, 0.57, 0.59, 0.61, 0.63, 0.65];% Comparison IOI: short(0.55, 0.57, 0.59), long(0.61, 0.63, 0.65)
beginning = [-0.18, 0 , 0.18]; % Beginning modulation: early(-0.18s),ontime(0s),late(0.18s)
delay = [sIOI+beginning;2*sIOI+beginning;3*sIOI+beginning]; % Delay duration: 2,3,4 IOI

nt = 0:1/fs:dur-1/fs;
N = length(nt);
Y_unramp = cos(2*pi*f0*nt);
rampDur = floor(0.005 *fs) - 1;
ramp_f = linspace(0, 1, rampDur);
ramp_b = linspace(1, 0, rampDur);
window = [ramp_f ones(1, N-2*rampDur) ramp_b];
Y = Y_unramp.* window;

standard_tr = [Y zeros(1,round(fs*(sIOI-dur)))]; % the whole IOI
int1_standard = repmat(standard_tr,1,2);
int6_standard = repmat(standard_tr,1,7);
comparison_1 = [Y zeros(1,round(fs*(cIOI(1)-dur)))];
comparison1 = repmat(comparison_1,1,2);
comparison_2 = [Y zeros(1,round(fs*(cIOI(2)-dur)))];
comparison2 = repmat(comparison_2,1,2);
comparison_3 = [Y zeros(1,round(fs*(cIOI(3)-dur)))];
comparison3 = repmat(comparison_3,1,2);
comparison_4 = [Y zeros(1,round(fs*(cIOI(4)-dur)))];
comparison4 = repmat(comparison_4,1,2);
comparison_5 = [Y zeros(1,round(fs*(cIOI(5)-dur)))];
comparison5 = repmat(comparison_5,1,2);
comparison_6 = [Y zeros(1,round(fs*(cIOI(6)-dur)))];
comparison6 = repmat(comparison_6,1,2);

std_int = {int1_standard;int6_standard};
comp_int = {comparison1,comparison2,comparison3,comparison4,comparison5,comparison6};

std_IOI = {'1','6'};
delay_dur = {'2','3','4'}; 
begin_time = {'5','8','9'}; % correspond to early(1), ontime(2), late(3) beginning
comp_IOI = {'1','2','3','4','5','6'}; % correspond to 0.55(1), 0.57(2), 0.59(3), 0.61(4), 0.63(5), 0.65(6) cIOI

for i = 1:length(std_IOI)
    for j = 1:length(delay_dur)
        for k = 1:length(begin_time)
            for l = 1:length(comp_IOI)
            filename = ['int' std_IOI{i} '_delay' delay_dur{j} '_' begin_time{k} comp_IOI{l} '.wav'];
            sound_file = [std_int{i} zeros(1,round(fs*delay(j,k))) comp_int{l}];
            audiowrite(filename,sound_file,fs);
            end
        end
    end
end