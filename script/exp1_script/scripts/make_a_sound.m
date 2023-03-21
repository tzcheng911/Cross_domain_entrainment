%% make a sound file
clear all
close all
clc

fs = 44100; %sampling rate
dur = 0.05; %duration
f0 = 200;
IOI = 0.25;

nt = 0:1/fs:dur-1/fs;
Y = cos(2*pi*f0*nt);

% figure; plot(nt,x); plot(abs(fft(x)))
% way1
% soundsc(x, fs); %normalize audioplayer
% sound(x,fs);

% way2
aud = audioplayer (Y,fs);
play(aud);

IOIN = fs*IOI - length(Y); % how many IOI
beat = [Y zeros(1,IOIN)];
subbeat = beat*0.1; %
trial = [beat subbeat];
%cyclesub = cycle*0.1;

stim = repmat(trial,1,10);

subplot(4,1,1);
plot(nt,Y);
subplot(4,1,2);
plot(beat);
subplot(4,1,3);
plot(subbeat);
subplot(4,1,4);
plot(stim);

filename = 'myfirstsoundfile.wav';
audiowrite(filename,stim,fs);

aud = audioplayer (stim,fs);
play(aud);
pause(aud);


