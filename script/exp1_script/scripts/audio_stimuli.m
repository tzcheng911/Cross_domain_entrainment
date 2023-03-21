clear all
close all
clc

fs = 44100; %sampling rate
dur = 0.08; %duration
f0 = 200;
sIOI = 0.2; %Standard IOI
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
int6_standard = repmat(standard_tr,1,6);
figure, plot(int6_standard,'k')
axis off
%%

comparison_tr1 = [Y zeros(1,round(fs*(cIOI(1)-dur)))];
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

% 18 conditions for comparison1
int1_delay2_51 = [int1_standard zeros(1,round(fs*delay(1,1))) comparison1];
int1_delay2_81 = [int1_standard zeros(1,round(fs*delay(1,2))) comparison1];
int1_delay2_91 = [int1_standard zeros(1,round(fs*delay(1,3))) comparison1];
int6_delay2_51 = [int6_standard zeros(1,round(fs*delay(1,1))) comparison1];
int6_delay2_81 = [int6_standard zeros(1,round(fs*delay(1,2))) comparison1];
int6_delay2_91 = [int6_standard zeros(1,round(fs*delay(1,3))) comparison1];
int1_delay3_51 = [int1_standard zeros(1,round(fs*delay(2,1))) comparison1];
int1_delay3_81 = [int1_standard zeros(1,round(fs*delay(2,2))) comparison1];
int1_delay3_91 = [int1_standard zeros(1,round(fs*delay(2,3))) comparison1];
int6_delay3_51 = [int6_standard zeros(1,round(fs*delay(2,1))) comparison1];
int6_delay3_81 = [int6_standard zeros(1,round(fs*delay(2,2))) comparison1];
int6_delay3_91 = [int6_standard zeros(1,round(fs*delay(2,3))) comparison1];
int1_delay4_51 = [int1_standard zeros(1,round(fs*delay(3,1))) comparison1];
int1_delay4_81 = [int1_standard zeros(1,round(fs*delay(3,2))) comparison1];
int1_delay4_91 = [int1_standard zeros(1,round(fs*delay(3,3))) comparison1];
int6_delay4_51 = [int6_standard zeros(1,round(fs*delay(3,1))) comparison1];
int6_delay4_81 = [int6_standard zeros(1,round(fs*delay(3,2))) comparison1];
int6_delay4_91 = [int6_standard zeros(1,round(fs*delay(3,3))) comparison1];

% 18 conditions for comparison2
int1_delay2_52 = [int1_standard zeros(1,round(fs*delay(1,1))) comparison2];
int1_delay2_82 = [int1_standard zeros(1,round(fs*delay(1,2))) comparison2];
int1_delay2_92 = [int1_standard zeros(1,round(fs*delay(1,3))) comparison2];
int6_delay2_52 = [int6_standard zeros(1,round(fs*delay(1,1))) comparison2];
int6_delay2_82 = [int6_standard zeros(1,round(fs*delay(1,2))) comparison2];
int6_delay2_92 = [int6_standard zeros(1,round(fs*delay(1,3))) comparison2];
int1_delay3_52 = [int1_standard zeros(1,round(fs*delay(2,1))) comparison2];
int1_delay3_82 = [int1_standard zeros(1,round(fs*delay(2,2))) comparison2];
int1_delay3_92 = [int1_standard zeros(1,round(fs*delay(2,3))) comparison2];
int6_delay3_52 = [int6_standard zeros(1,round(fs*delay(2,1))) comparison2];
int6_delay3_82 = [int6_standard zeros(1,round(fs*delay(2,2))) comparison2];
int6_delay3_92 = [int6_standard zeros(1,round(fs*delay(2,3))) comparison2];
int1_delay4_52 = [int1_standard zeros(1,round(fs*delay(3,1))) comparison2];
int1_delay4_82 = [int1_standard zeros(1,round(fs*delay(3,2))) comparison2];
int1_delay4_92 = [int1_standard zeros(1,round(fs*delay(3,3))) comparison2];
int6_delay4_52 = [int6_standard zeros(1,round(fs*delay(3,1))) comparison2];
int6_delay4_82 = [int6_standard zeros(1,round(fs*delay(3,2))) comparison2];
int6_delay4_92 = [int6_standard zeros(1,round(fs*delay(3,3))) comparison2];

% 18 conditions for comparison3
int1_delay2_53 = [int1_standard zeros(1,round(fs*delay(1,1))) comparison3];
int1_delay2_83 = [int1_standard zeros(1,round(fs*delay(1,2))) comparison3];
int1_delay2_93 = [int1_standard zeros(1,round(fs*delay(1,3))) comparison3];
int6_delay2_53 = [int6_standard zeros(1,round(fs*delay(1,1))) comparison3];
int6_delay2_83 = [int6_standard zeros(1,round(fs*delay(1,2))) comparison3];
int6_delay2_93 = [int6_standard zeros(1,round(fs*delay(1,3))) comparison3];
int1_delay3_53 = [int1_standard zeros(1,round(fs*delay(2,1))) comparison3];
int1_delay3_83 = [int1_standard zeros(1,round(fs*delay(2,2))) comparison3];
int1_delay3_93 = [int1_standard zeros(1,round(fs*delay(2,3))) comparison3];
int6_delay3_53 = [int6_standard zeros(1,round(fs*delay(2,1))) comparison3];
int6_delay3_83 = [int6_standard zeros(1,round(fs*delay(2,2))) comparison3];
int6_delay3_93 = [int6_standard zeros(1,round(fs*delay(2,3))) comparison3];
int1_delay4_53 = [int1_standard zeros(1,round(fs*delay(3,1))) comparison3];
int1_delay4_83 = [int1_standard zeros(1,round(fs*delay(3,2))) comparison3];
int1_delay4_93 = [int1_standard zeros(1,round(fs*delay(3,3))) comparison3];
int6_delay4_53 = [int6_standard zeros(1,round(fs*delay(3,1))) comparison3];
int6_delay4_83 = [int6_standard zeros(1,round(fs*delay(3,2))) comparison3];
int6_delay4_93 = [int6_standard zeros(1,round(fs*delay(3,3))) comparison3];

% 18 conditions for comparison4
int1_delay2_54 = [int1_standard zeros(1,round(fs*delay(1,1))) comparison4];
int1_delay2_84 = [int1_standard zeros(1,round(fs*delay(1,2))) comparison4];
int1_delay2_94 = [int1_standard zeros(1,round(fs*delay(1,3))) comparison4];
int6_delay2_54 = [int6_standard zeros(1,round(fs*delay(1,1))) comparison4];
int6_delay2_84 = [int6_standard zeros(1,round(fs*delay(1,2))) comparison4];
int6_delay2_94 = [int6_standard zeros(1,round(fs*delay(1,3))) comparison4];
int1_delay3_54 = [int1_standard zeros(1,round(fs*delay(2,1))) comparison4];
int1_delay3_84 = [int1_standard zeros(1,round(fs*delay(2,2))) comparison4];
int1_delay3_94 = [int1_standard zeros(1,round(fs*delay(2,3))) comparison4];
int6_delay3_54 = [int6_standard zeros(1,round(fs*delay(2,1))) comparison4];
int6_delay3_84 = [int6_standard zeros(1,round(fs*delay(2,2))) comparison4];
int6_delay3_94 = [int6_standard zeros(1,round(fs*delay(2,3))) comparison4];
int1_delay4_54 = [int1_standard zeros(1,round(fs*delay(3,1))) comparison4];
int1_delay4_84 = [int1_standard zeros(1,round(fs*delay(3,2))) comparison4];
int1_delay4_94 = [int1_standard zeros(1,round(fs*delay(3,3))) comparison4];
int6_delay4_54 = [int6_standard zeros(1,round(fs*delay(3,1))) comparison4];
int6_delay4_84 = [int6_standard zeros(1,round(fs*delay(3,2))) comparison4];
int6_delay4_94 = [int6_standard zeros(1,round(fs*delay(3,3))) comparison4];

% 18 conditions for comparison5
int1_delay2_55 = [int1_standard zeros(1,round(fs*delay(1,1))) comparison5];
int1_delay2_85 = [int1_standard zeros(1,round(fs*delay(1,2))) comparison5];
int1_delay2_95 = [int1_standard zeros(1,round(fs*delay(1,3))) comparison5];
int6_delay2_55 = [int6_standard zeros(1,round(fs*delay(1,1))) comparison5];
int6_delay2_85 = [int6_standard zeros(1,round(fs*delay(1,2))) comparison5];
int6_delay2_95 = [int6_standard zeros(1,round(fs*delay(1,3))) comparison5];
int1_delay3_55 = [int1_standard zeros(1,round(fs*delay(2,1))) comparison5];
int1_delay3_85 = [int1_standard zeros(1,round(fs*delay(2,2))) comparison5];
int1_delay3_95 = [int1_standard zeros(1,round(fs*delay(2,3))) comparison5];
int6_delay3_55 = [int6_standard zeros(1,round(fs*delay(2,1))) comparison5];
int6_delay3_85 = [int6_standard zeros(1,round(fs*delay(2,2))) comparison5];
int6_delay3_95 = [int6_standard zeros(1,round(fs*delay(2,3))) comparison5];
int1_delay4_55 = [int1_standard zeros(1,round(fs*delay(3,1))) comparison5];
int1_delay4_85 = [int1_standard zeros(1,round(fs*delay(3,2))) comparison5];
int1_delay4_95 = [int1_standard zeros(1,round(fs*delay(3,3))) comparison5];
int6_delay4_55 = [int6_standard zeros(1,round(fs*delay(3,1))) comparison5];
int6_delay4_85 = [int6_standard zeros(1,round(fs*delay(3,2))) comparison5];
int6_delay4_95 = [int6_standard zeros(1,round(fs*delay(3,3))) comparison5];

% 18 conditions for comparison6
int1_delay2_56 = [int1_standard zeros(1,round(fs*delay(1,1))) comparison6];
int1_delay2_86 = [int1_standard zeros(1,round(fs*delay(1,2))) comparison6];
int1_delay2_96 = [int1_standard zeros(1,round(fs*delay(1,3))) comparison6];
int6_delay2_56 = [int6_standard zeros(1,round(fs*delay(1,1))) comparison6];
int6_delay2_86 = [int6_standard zeros(1,round(fs*delay(1,2))) comparison6];
int6_delay2_96 = [int6_standard zeros(1,round(fs*delay(1,3))) comparison6];
int1_delay3_56 = [int1_standard zeros(1,round(fs*delay(2,1))) comparison6];
int1_delay3_86 = [int1_standard zeros(1,round(fs*delay(2,2))) comparison6];
int1_delay3_96 = [int1_standard zeros(1,round(fs*delay(2,3))) comparison6];
int6_delay3_56 = [int6_standard zeros(1,round(fs*delay(2,1))) comparison6];
int6_delay3_86 = [int6_standard zeros(1,round(fs*delay(2,2))) comparison6];
int6_delay3_96 = [int6_standard zeros(1,round(fs*delay(2,3))) comparison6];
int1_delay4_56 = [int1_standard zeros(1,round(fs*delay(3,1))) comparison6];
int1_delay4_86 = [int1_standard zeros(1,round(fs*delay(3,2))) comparison6];
int1_delay4_96 = [int1_standard zeros(1,round(fs*delay(3,3))) comparison6];
int6_delay4_56 = [int6_standard zeros(1,round(fs*delay(3,1))) comparison6];
int6_delay4_86 = [int6_standard zeros(1,round(fs*delay(3,2))) comparison6];
int6_delay4_96 = [int6_standard zeros(1,round(fs*delay(3,3))) comparison6];

%% play to hear how the waves sound like
%aud = audioplayer (int1_delay2_otrl,fs);
%play(aud);
%pause(aud);

std_IOI = {'1','6'};
delay_dur = {'2','3','4'}; 
begin_time = {'5','8','9'}; % correspond to early(1), ontime(2), late(3) beginning
comp_IOI = {'1','2','3','4','5','6'}; % correspond to 0.55(1), 0.57(2), 0.59(3), 0.61(4), 0.63(5), 0.65(6) cIOI

for i = 1:2
    for j = 1:3
        for k = 1:3
            for l = 1:6
            filename = ['int' std_IOI{i} '_delay' delay_dur{j} '_' begin_time{k} comp_IOI{l} '.wav'];
            eval ([filename '= filename']);
            %audiowrite(filename,eval ([filename '= filename']),fs);
            end
        end
    end
end