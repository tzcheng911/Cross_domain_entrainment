%adding a ramp window to audio
%50ms tone played at 440Hz with a 5ms onset ramp

fs = 44100;
f = 440;
dur = 0.05;
nT = 0: (1/fs) : dur - (1/fs);
N = length(nT);
x = cos(2*pi*f*nT);
 
rampDur = floor(0.005 *fs) - 1;
ramp = linspace(0, 1, rampDur);
window = [ramp ones(1, N-rampDur)];
x2 = x .* window;

figure; subplot(2,1,1);
plot(nT, x); hold on; plot(nT, window);
subplot(2,1,2);
plot(nT,x2)
