cd /Users/tzu-hancheng/Google_Drive/Research/entrainedVOT/real2019/stimuli/praat/cutfromfront/8VOTs
%% convert from stereo to mono and rescale the loudness: *** could be done in praat as a standard process
clear
clc
fileDir = '/Users/tzu-hancheng/Google_Drive/Research/entrainedVOT/real2019/stimuli/praat/cutfromfront/8VOTs';
files = dir(fullfile(fileDir,'*.wav'));
files = {files.name};
for i = 1:length(files)
    [temp_audio,fs] = audioread(files{i});
    filename = [files{i}];
    mono = temp_audio(:,1);
    rescale_mono = rescale(mono,-0.5,0.5);
    audiowrite(filename,rescale_mono,fs);
    clear temp_audio
end

%% parse the sound files with context
cd('/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/VOTs/pilot2018/stimuli/finalized_stimuli/mono')
clear 
close all
clc

fs = 44100; %sampling rate
dur = 0.06; %duration
f0 = 440;
sIOI = 0.60; %Standard IOI
%cIOI = [0.55, 0.57, 0.59, 0.61, 0.63, 0.65];% Comparison IOI: short(0.55, 0.57, 0.59), long(0.61, 0.63, 0.65)
VOT = [1:8];
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
VOT_1 = audioread('VOT1.wav');
VOT_2 = audioread('VOT2.wav');
VOT_3 = audioread('VOT3.wav');
VOT_4 = audioread('VOT4.wav');
VOT_5 = audioread('VOT5.wav');
VOT_6 = audioread('VOT6.wav');
VOT_7 = audioread('VOT7.wav');
VOT_8 = audioread('VOT8.wav');

pulse = [Y zeros(1,round(fs*(sIOI-dur)))]; % the whole IOI
context = repmat(pulse,1,5);

%%
syllable = {VOT_1',VOT_2',VOT_3',VOT_4',VOT_5',VOT_6',VOT_7',VOT_8'};
begin_time = {'early','ontime','late'}; % correspond to early(1), ontime(2), late(3) beginning
VOTs = {'VOT1','VOT2','VOT3','VOT4','VOT5','VOT6','VOT7','VOT8'}; 
        for k = 1:length(begin_time)
            for l = 1:length(syllable)
                filename = [begin_time{k} '_' VOTs{l} '_1.wav'];
                sound_file = [context Y zeros(1,round(fs*(sIOI-dur+beginning(k)))) syllable{l}];
                audiowrite(filename,sound_file,fs);
            end
        end
        
ontime = audioread('ontime_VOT1.mp3');
early = audioread('early_VOT1.mp3');
late = audioread('late_VOT1.mp3');
subplot(3,1,1)
plot(early)
subplot(3,1,2)
plot(ontime)
subplot(3,1,3)
plot(late)
