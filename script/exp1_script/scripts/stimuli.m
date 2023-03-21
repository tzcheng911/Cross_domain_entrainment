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
comparison_tr = [Y zeros(1,round(fs*(cIOI(1)-dur)))];
comparison = repmat(comparison_tr,1,2);
% 18 conditions
int1_delay2_etrl = [int1_standard zeros(1,round(fs*delay(1,1))) comparison];
int1_delay2_otrl = [int1_standard zeros(1,round(fs*delay(1,2))) comparison];
int1_delay2_ltrl = [int1_standard zeros(1,round(fs*delay(1,3))) comparison];
int6_delay2_etrl = [int6_standard zeros(1,round(fs*delay(1,1))) comparison];
int6_delay2_otrl = [int6_standard zeros(1,round(fs*delay(1,2))) comparison];
int6_delay2_ltrl = [int6_standard zeros(1,round(fs*delay(1,3))) comparison];
int1_delay3_etrl = [int1_standard zeros(1,round(fs*delay(2,1))) comparison];
int1_delay3_otrl = [int1_standard zeros(1,round(fs*delay(2,2))) comparison];
int1_delay3_ltrl = [int1_standard zeros(1,round(fs*delay(2,3))) comparison];
int6_delay3_etrl = [int6_standard zeros(1,round(fs*delay(2,1))) comparison];
int6_delay3_otrl = [int6_standard zeros(1,round(fs*delay(2,2))) comparison];
int6_delay3_ltrl = [int6_standard zeros(1,round(fs*delay(2,3))) comparison];
int1_delay4_etrl = [int1_standard zeros(1,round(fs*delay(3,1))) comparison];
int1_delay4_otrl = [int1_standard zeros(1,round(fs*delay(3,2))) comparison];
int1_delay4_ltrl = [int1_standard zeros(1,round(fs*delay(3,3))) comparison];
int6_delay4_etrl = [int6_standard zeros(1,round(fs*delay(3,1))) comparison];
int6_delay4_otrl = [int6_standard zeros(1,round(fs*delay(3,2))) comparison];
int6_delay4_ltrl = [int6_standard zeros(1,round(fs*delay(3,3))) comparison];

%% plot to see how the waves look like
close all
subplot (9,2,1);
plot(int1_delay2_etrl);
subplot (9,2,3);
plot(int1_delay2_otrl);
subplot (9,2,5);
plot(int1_delay2_ltrl);
subplot (9,2,7);
plot(int1_delay3_etrl);
subplot (9,2,9);
plot(int1_delay3_otrl);
subplot (9,2,11);
plot(int1_delay3_ltrl);
subplot (9,2,13);
plot(int1_delay4_etrl);
subplot (9,2,15);
plot(int1_delay4_otrl);
subplot (9,2,17);
plot(int1_delay4_ltrl);
subplot (9,2,2);
plot(int6_delay2_etrl);
subplot (9,2,4);
plot(int6_delay2_otrl);
subplot (9,2,6);
plot(int6_delay2_ltrl);
subplot (9,2,8);
plot(int6_delay3_etrl);
subplot (9,2,10);
plot(int6_delay3_otrl);
subplot (9,2,12);
plot(int6_delay3_ltrl);
subplot (9,2,14);
plot(int6_delay4_etrl);
subplot (9,2,16);
plot(int6_delay4_otrl);
subplot (9,2,18);
plot(int6_delay4_ltrl);
%% play to hear how the waves sound like
aud = audioplayer (int1_delay2_otrl,fs);
play(aud);
%pause(aud);

filename = 'int1_delay2_otrl.wav';
audiowrite(filename,int1_delay2_otrl,fs);