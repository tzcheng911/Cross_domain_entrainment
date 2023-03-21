addpath('/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp3_20CR11/script')
clear 
clc

%% parse the VOT after context tones
cd('/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp7_21CR04_VOT/FF2021/stimuli/rescale_mono_continuum') % VOT stimuli

% parameters to customize a tone (300 ms or 600 ms)
fs = 44100; % sampling rate
f0 = 440;
dur = .06; % All stimulus sequences were comprised of a series of 60-ms tones
sIOI = .30; % Standard duration
step = .01; % step of the comparison interval
beginning = [-0.09, 0 , 0.09]; % Beginning modulation: early(-0.18s),ontime(0s),late(0.18s)
rampDur = 0.005;
delay = [sIOI+beginning;3*sIOI+beginning;5*sIOI+beginning]; % Delay duration: 2 or 4 or 8 IOI +/- 0.09
win_choice = 'linear'; 

% Generate the context tones (empty), standard tones (filled), comparison
% tones(filled)
pure_tone = generate_tone(f0, fs, dur, sIOI, rampDur, win_choice);
context_tone = [pure_tone zeros(1,round(fs*(sIOI-dur)))];
context = repmat(context_tone,1,6);

VOTs = {'Emily_pa5_mono','Emily_pa6_mono','Emily_pa7_mono','Emily_pa8_mono','Emily_pa9_mono','Emily_pa10_mono','Emily_pa11_mono','Emily_pa12_mono'}; 

cont1 = audioread('Emily_pa5_mono.wav');
cont2 = audioread('Emily_pa6_mono.wav');
cont3 = audioread('Emily_pa7_mono.wav');
cont4 = audioread('Emily_pa8_mono.wav');
cont5 = audioread('Emily_pa9_mono.wav');
cont6 = audioread('Emily_pa10_mono.wav');
cont7 = audioread('Emily_pa11_mono.wav');
cont8 = audioread('Emily_pa12_mono.wav');

cd('/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp7_21CR04_VOT/FF2021/FF_stimuli_upload')
begin_time = {'early','ontime','late'}; % correspond to early(1), ontime(2), late(3) beginning
delay_dur = {'2','4','8'}; 
syllable = {cont1',cont2',cont3',cont4',cont5',cont6',cont7',cont8'};

% concatenate and save the files
for k = 1:length(begin_time)
    for l = 1:length(VOTs)
        for m = 1
            filename = ['300ms_' begin_time{k} '_' 'delay_' delay_dur{m} '_' VOTs{l} '.wav'];
%            sound_file = [context zeros(1,round(fs*delay(1,2)*0.5)) syllable{l}]; 
%            rsyllable = rescale(syllable{l},-1,1); % rescale to be between
%            -1 and 1 but have clipping
            sound_file = [context zeros(1,round(fs*delay(m,k))) syllable{l}]; % correct by Zoe 2022/02/15
            audiowrite(filename,sound_file,fs);
        end
    end
end

%% parse the VOT after context tones
cd('/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp6_21CR03_Vowel_length/FF2021/stimuli') % VL stimuli

VLs = {'Sarah_Lab_1','Sarah_Lab_2','Sarah_Lab_3','Sarah_Lab_4','Sarah_Lab_5','Sarah_Lab_6','Sarah_Lab_7','Sarah_Lab_8'}; 

cont1 = audioread('Sarah_Lab_1.wav');
cont2 = audioread('Sarah_Lab_2.wav');
cont3 = audioread('Sarah_Lab_3.wav');
cont4 = audioread('Sarah_Lab_4.wav');
cont5 = audioread('Sarah_Lab_5.wav');
cont6 = audioread('Sarah_Lab_6.wav');
cont7 = audioread('Sarah_Lab_7.wav');
cont8 = audioread('Sarah_Lab_8.wav');

begin_time = {'early','ontime','late'}; % correspond to early(1), ontime(2), late(3) beginning
delay_dur = {'2','4','8'}; 
syllable = {cont1',cont2',cont3',cont4',cont5',cont6',cont7',cont8'};

% concatenate and save the files
for k = 1:length(begin_time)
    for l = 1:length(VLs)
        for m = 1
            filename = ['300ms_' begin_time{k} '_' 'delay_' delay_dur{m} '_' VLs{l} '.wav'];
%            sound_file = [context zeros(1,round(fs*delay(1,2)*0.5)) syllable{l}];
%            rsyllable = rescale(syllable{l},-1,1); % rescale to be between
%            -1 and 1 but have clipping
            sound_file = [context zeros(1,round(fs*delay(m,k))) syllable{l}]; % correct by Zoe 2022/02/15
            audiowrite(filename,sound_file,fs);
        end
    end
end

%% visualization      
VL_ontime = audioread('/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp6_21CR03_Vowel_length/FF2021/stimuli/300ms_early_delay_2_Sarah_Lab_8.wav');
VOT_ontime = audioread('/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp7_21CR04_VOT/FF2021/stimuli/300ms_early_delay_2_Emily_pa5_mono.wav');
tone_ontime = audioread('/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp4_20CR12/stimuli/single_300ms_delay_2_51.wav');
figure
plot(VL_ontime)
hold on
plot(tone_ontime)
