clear all
close all
clc

fs = 44100; %sampling rate
dur = 0.5; %duration
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
int2_standard = repmat(standard_tr,1,2);

p = audioplayer(int2_standard, fs);
        play(p);
 