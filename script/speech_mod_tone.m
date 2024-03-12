%% 
% Created 2022/2 for exp4c speech modified tone stimuli
% Modified 2023/10 for exp8 speech modified tone stimuli (upper and lower env)

clear 
close all
clc
addpath('/Users/t.z.cheng/Documents/GitHub/Cross_domain_entrainment/script')
%% Create single speech modified tone for each lab-lap step: just the upper envelope
% cd('/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp6_21CR03_Vowel_length/FF2021/stimuli')
% 
% allspeech = dir('Sarah*');
% 
% for i = 1:length(allspeech)
%     cd('/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp6_21CR03_Vowel_length/FF2021/stimuli')
%     name = allspeech(i).name;
%     [speech,fs] = audioread(name);
% 
%     %% Generate the sine wave tones length-matching the steps
%     f0 = 440;
%     sIOI = 0;
%     rampDur = 0.005;
%     win_choice = 'linear'; 
%     dur = length(speech)/fs;
%     pure_tone = generate_tone(f0, fs, dur, sIOI, rampDur, win_choice);
%     [env_up,env_lo] = envelope(speech,200,'peak');
%     env_up(env_up < 0) = 0; % force the envelope to be zero
%     mod_pure_tone = pure_tone(:).*env_up(:);    
%     r_mod_pure_tone = rescale(mod_pure_tone,-1,1); % rescale to be between -1 and 1
%     cd('/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp4_20CR12/4c')
%     audiowrite(strcat('tone_',name(7:end)),r_mod_pure_tone,fs)
% end

%% Create single speech modified tone for each add-at step: average speech envelope
cd('/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp8b/stimuli/test_stimuli')
allspeech = dir('a*');

for i = 1:length(allspeech)
    % name = allspeech(i).name;
    % [speech,fs] = audioread(name);
    % for the six pairs
    name = {'Back.wav','Bag.wav','Bead.wav','Beat.wav','Cab.wav','Cap.wav',...
        'Fad.wav','Fat.wav','Lab.wav','Lap.wav','Lag.wav','Lack.wav'};
    [speech,fs] = audioread(name{i});

    %% Generate the sine wave tones length-matching the steps
    f0 = 440;
    sIOI = 0;
    rampDur = 0.005;
    win_choice = 'linear'; 
    dur = length(speech)/fs;
    pure_tone = generate_tone(f0, fs, dur, sIOI, rampDur, win_choice);
    speech_r = abs(speech);
    
    % Traditional rectified speech
    [env_up,env_lo] = envelope(speech_r,200,'peak'); 
    env_avg = 0.5*(env_up+(-1*env_lo));
    % env_up(env_up < 0) = 0; % force the envelope >=0
    mod_pure_tone = pure_tone(:).*env_avg(:);    
    
%     audiowrite(strcat('avg_env_tone_',name(2:end)),mod_pure_tone,fs)
%     audiowrite(strcat('speech_',name(2:end)),speech,fs)
    
% for the six pairs    
    audiowrite(strcat('avg_env_tone_',name{i}),mod_pure_tone,fs)
    audiowrite(strcat('speech_',name{i}),speech,fs)
end

%% Visualize the env
[speech,fs] = audioread('a1.wav');
[tone,fs] = audioread('rect_tone_1.wav');

speech_r = abs(speech);
[env_r_speech,~] = envelope(speech_r,200,'peak'); % only the upper envelope
env_r_speech(env_r_speech < 0) = 0; % force the envelope >=0

[env_up,env_lo] = envelope(tone,200,'peak'); 
env_up(env_up < 0) = 0; % force the upper envelope >=0
env_lo(env_lo > 0) = 0; % force the lower envelope <=0

figure;plot(speech)
hold on;plot(env_r_speech,'LineWidth',2,'color','k')
hold on;plot(-env_r_speech,'LineWidth',2,'color','k')
hold on;plot(env_up,'red','LineWidth',2,'LineStyle','--')
hold on;plot(env_lo,'red','LineWidth',2,'LineStyle','--')
legend('Speech sound','Rect speech env','-Rect speech env','Tone up env','Tone low env')
xlim([0 length(tone)])
xlabel('Time (sample)')
ylabel('Amplitude')
title('Step 1')

%% Create single speech modified tone for each add-at step: both upper and lower envelope
% drawback: additional harmonics
% cd('/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp8b/stimuli/test_stimuli')
% allspeech = dir('a*');
% addpath('/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/Delaydoesmatter/real_exp/exp3_20CR11/script')
% for i = 1:length(allspeech)
%     name = allspeech(i).name;
%     [speech,fs] = audioread(name);
%     %% Generate the sine wave tones length-matching the steps
%     f0 = 440;
%     sIOI = 0;
%     rampDur = 0.005;
%     win_choice = 'linear'; 
%     dur = length(speech)/fs;
%     pure_tone = generate_tone(f0, fs, dur, sIOI, rampDur, win_choice);
% %     nt = 0:1/fs:dur-1/fs;
% %     pure_tone = sin(2*pi*f0*nt);
%     [env_up,env_lo] = envelope(speech,200,'peak');
%     mod_pure_tone = [];
%     for nt = 1:length(pure_tone)
%         if pure_tone(nt) > 0
%             mod_pure_tone(nt) = pure_tone(nt)*env_up(nt);
%         elseif pure_tone(nt) < 0
%             mod_pure_tone(nt) = pure_tone(nt)*-env_lo(nt); % reverse the sign
%         end
%     end
%     audiowrite(strcat('tone_',name),mod_pure_tone,fs)
%     audiowrite(strcat('tone',name(2:end)),pure_tone,fs)
% end

%% plot them to check the timing ***very important***
% cd('/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp4_20CR12/4c/FF_stimuli_upload')
% cd('/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp6_21CR03_Vowel_length/FF2021/FF_stimuli_upload')

% figure;
% allspeech = dir('300ms_late*');
% for i = 1:length(allspeech)
%     name = allspeech(i).name;
%     [tone,fs] = audioread(name);
%     hold on; plot(tone)
% end