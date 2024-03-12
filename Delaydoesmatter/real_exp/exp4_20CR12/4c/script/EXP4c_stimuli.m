%% 
% Created 2022/2 for exp4c speech modified tone stimuli
% Modified 2023/10 for exp8 speech modified tone stimuli (upper and lower env)
% Modified 2023/11 for exp8b speech modified tone stimuli (upper and lower env)


clear 
clc
addpath('/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp3_20CR11/script')


%% Create single speech modified tone for each lab-lap step: just the upper envelope
cd('/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp6_21CR03_Vowel_length/FF2021/stimuli')

allspeech = dir('Sarah*');

for i = 1:length(allspeech)
    cd('/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp6_21CR03_Vowel_length/FF2021/stimuli')
    name = allspeech(i).name;
    [speech,fs] = audioread(name);

    %% Generate the sine wave tones length-matching the steps
    f0 = 440;
    sIOI = 0;
    rampDur = 0.005;
    win_choice = 'linear'; 
    dur = length(speech)/fs;
    pure_tone = generate_tone(f0, fs, dur, sIOI, rampDur, win_choice);
    [env_up,env_lo] = envelope(speech,200,'peak');
    env_up(env_up < 0) = 0; % force the envelope to be zero
    mod_pure_tone = pure_tone(:).*env_up(:);    
    r_mod_pure_tone = rescale(mod_pure_tone,-1,1); % rescale to be between -1 and 1
    cd('/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp4_20CR12/4c')
    audiowrite(strcat('tone_',name(7:end)),r_mod_pure_tone,fs)
end

%% Create single speech modified tone for each add-at step: just the upper envelope
cd('/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp6_21CR03_Vowel_length/FF2021/stimuli')

allspeech = dir('Sarah*');

for i = 1:length(allspeech)
    cd('/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp6_21CR03_Vowel_length/FF2021/stimuli')
    name = allspeech(i).name;
    [speech,fs] = audioread(name);

    %% Generate the sine wave tones length-matching the steps
    f0 = 440;
    sIOI = 0;
    rampDur = 0.005;
    win_choice = 'linear'; 
    dur = length(speech)/fs;
    pure_tone = generate_tone(f0, fs, dur, sIOI, rampDur, win_choice);
    [env_up,env_lo] = envelope(speech,200,'peak');
    env_up(env_up < 0) = 0; % force the envelope to be zero
    mod_pure_tone = pure_tone(:).*env_up(:);    
    r_mod_pure_tone = rescale(mod_pure_tone,-1,1); % rescale to be between -1 and 1
    cd('/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp4_20CR12/4c')
    audiowrite(strcat('tone_',name(7:end)),r_mod_pure_tone,fs)
end

%% Create single speech modified tone for each add-at step: both upper and lower envelope
cd('/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp8b/stimuli/test_stimuli')
allspeech = dir('a*');
addpath('/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/Delaydoesmatter/real_exp/exp3_20CR11/script')
for i = 1:length(allspeech)
    name = allspeech(i).name;
    [speech,fs] = audioread(name);
    %% Generate the sine wave tones length-matching the steps
    f0 = 440;
    sIOI = 0;
    rampDur = 0.005;
    win_choice = 'linear'; 
    dur = length(speech)/fs;
    pure_tone = generate_tone(f0, fs, dur, sIOI, rampDur, win_choice);
%     nt = 0:1/fs:dur-1/fs;
%     pure_tone = sin(2*pi*f0*nt);
    [env_up,env_lo] = envelope(speech,200,'peak');
    mod_pure_tone = [];
    for nt = 1:length(pure_tone)
        if pure_tone(nt) > 0
            mod_pure_tone(nt) = pure_tone(nt)*env_up(nt);
        elseif pure_tone(nt) < 0
            mod_pure_tone(nt) = pure_tone(nt)*-env_lo(nt); % reverse the sign
        end
    end
    audiowrite(strcat('tone_',name),mod_pure_tone,fs)
    audiowrite(strcat('tone',name(2:end)),pure_tone,fs)
end

%% plot them to check the timing ***very important***
% cd('/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp4_20CR12/4c/FF_stimuli_upload')
% cd('/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp6_21CR03_Vowel_length/FF2021/FF_stimuli_upload')

figure;
allspeech = dir('300ms_late*');
for i = 1:length(allspeech)
    name = allspeech(i).name;
    [tone,fs] = audioread(name);
    hold on; plot(tone)
end

%% Create context + single tone

% parameters to customize a tone (300 ms or 600 ms)
fs = 44100; % sampling rate
f0 = 440;
dur = .06; % All stimulus sequences were comprised of a series of 60-ms tones
sIOI = .30; % Standard duration
beginning = [-0.09, 0 , 0.09]; 
rampDur = 0.005;
delay = sIOI+beginning; % Delay duration: 2 or 4 or 8 IOI +/- 0.18
win_choice = 'linear'; 

% Generate the context tones (empty), standard tones (filled), comparison
% tones(filled)
pure_tone = generate_tone(f0, fs, dur, sIOI, rampDur, win_choice);
context_tone = [pure_tone zeros(1,round(fs*(sIOI-dur)))];
context = repmat(context_tone,1,6);

begin_time = {'early','ontime','late'}; % correspond to early(1), ontime(2), late(3) beginning
delay_dur = {'2'}; 

% cd('/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp4_20CR12/4c/stimuli')
cd('/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp8b/stimuli/official_stimuli/avg_env_tone_target_85db')
allmtones = dir('avg*');
for i = 1:length(allmtones)
    name = allmtones(i).name;
    [mtone,fs] = audioread(name);
    for j = 1:length(begin_time)
        filename = strcat('300ms_',begin_time{j},'_delay_',delay_dur,'_',name);
        sound_file = [context zeros(1,round(fs*delay(j))) mtone'];
        audiowrite(filename{1},sound_file,fs);
    end
end
      