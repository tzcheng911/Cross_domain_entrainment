%% parse the sound files with context
cd /Users/tzu-hancheng/Google_Drive/Research/cross_domain_entrainment/Vowel_length/pilot
clear 
close all
clc

fs = 44100; %sampling rate
dur = 0.06; %duration
f0 = 440;
sIOI = 0.60; %Standard IOI
%cIOI = [0.55, 0.57, 0.59, 0.61, 0.63, 0.65];% Comparison IOI: short(0.55, 0.57, 0.59), long(0.61, 0.63, 0.65)
VL = [1:8];
beginning = [-0.18, 0 , 0.18]; % Beginning modulation: early(-0.18s),ontime(0s),late(0.18s)

nt = 0:1/fs:dur-1/fs;
N = length(nt);
Y_unramp = cos(2*pi*f0*nt);
rampDur = floor(0.005 *fs) - 1;
ramp_f = linspace(0, 1, rampDur);
ramp_b = linspace(1, 0, rampDur);
window = [ramp_f ones(1, N-2*rampDur) ramp_b];
Y = Y_unramp.* window;
Y = 0.2*Y;
VL_1 = audioread('Lab_1.wav');
VL_2 = audioread('Lab_2.wav');
VL_3 = audioread('Lab_3.wav');
VL_4 = audioread('Lab_4.wav');
VL_5 = audioread('Lab_5.wav');
VL_6 = audioread('Lab_6.wav');
VL_7 = audioread('Lab_7.wav');
VL_8 = audioread('Lab_8.wav');

pulse = [Y zeros(1,round(fs*(sIOI-dur)))]; % the whole IOI
context = repmat(pulse,1,5);

%%
syllable = {VL_1',VL_2',VL_3',VL_4',VL_5',VL_6',VL_7',VL_8'};
begin_time = {'early','ontime','late'}; % correspond to early(1), ontime(2), late(3) beginning
VLs = {'Lab_1','Lab_2','Lab_3','Lab_4','Lab_5','Lab_6','Lab_7','Lab_8'}; 
        for k = 1:length(begin_time)
            for l = 1:length(syllable)
                filename = [begin_time{k} '_' VLs{l} '.wav'];
                sound_file = [context Y zeros(1,round(fs*(sIOI-dur+beginning(k)))) syllable{l}];
                audiowrite(filename,sound_file,fs);
            end
        end

%%
ontime = audioread('ontime_Lab_1.wav');
early = audioread('early_Lab_1.wav');
late = audioread('late_Lab_1.wav');
subplot(3,1,1)
plot(early)
subplot(3,1,2)
plot(ontime)
subplot(3,1,3)
plot(late)
