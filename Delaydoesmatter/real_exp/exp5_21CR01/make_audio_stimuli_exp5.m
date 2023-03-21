%% Empty tones: Make 1 (context tones) + 1 (standard) + 6 (comparison) durations 
clear 
close all
clc

% parameters to customize a tone (300 ms or 600 ms)
fs = 44100; % sampling rate
f0 = 440;
dur = .06; % All stimulus sequences were comprised of a series of 60-ms tones
sIOI = .30; % Standard duration
step = .01; % step of the comparison interval
%cIOI = 0.275:step:0.325; % Comparison duration
cIOI = [0.15,0.45]; % examples 0.15, 0.45

beginning = [-0.09, 0 , 0.09]; % Beginning modulation: early(-0.18s),ontime(0s),late(0.18s)
rampDur = 0.005;
delay = [1*sIOI+beginning;3*sIOI+beginning;7*sIOI+beginning]; % Delay duration: 2 or 4 or 8 IOI +/- 0.18
win_choice = 'linear'; 

% Generate the context tones (empty), standard tones (filled), comparison
% tones(filled)
pure_tone = generate_tone(f0, fs, dur, sIOI, rampDur, win_choice);
context_tone = [pure_tone zeros(1,round(fs*(sIOI-dur)))];
context = repmat(context_tone,1,6);
standard = repmat(context_tone,1,2);
for i = 1:length(cIOI)
    temp = [pure_tone zeros(1,round(fs*(cIOI(i)-dur)))];
    double_temp = repmat(temp,1,2);
    comparison(i) = {double_temp};
    clear temp
end

%%
cd('/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp5_21CR01/stimuli/empty_duration')

begin_time = {'5','8','9'}; % correspond to early(1), ontime(2), late(3) beginning
comp_IOI = {'1','2','3','4','5','6'}; % correspond to 0.275, 0.285, 0.295, 0.305, 0.315, 0.325 cIOI
delay_dur = {'2','4','8'}; 
%%
        for k = 1:length(begin_time)
            for l = 1:length(comp_IOI)
                for m = 1:length(delay_dur)
                filename = ['300ms_' 'delay_' delay_dur{m} '_' begin_time{k} comp_IOI{l} '.wav'];
                sound_file = [context zeros(1,round(fs*delay(1,2))) standard zeros(1,round(fs*delay(m,k))) comparison{l}];
                audiowrite(filename,sound_file,fs);
                end
            end
        end
        
%%
filename = ['e300ms_short.wav'];
sound_file = [context zeros(1,round(fs*delay(1,2))) standard zeros(1,round(fs*delay(1,2))) comparison{1}];
audiowrite(filename,sound_file,fs);

filename = ['e300ms_long.wav'];
sound_file = [context zeros(1,round(fs*delay(1,2))) standard zeros(1,round(fs*delay(1,2))) comparison{2}];
audiowrite(filename,sound_file,fs);