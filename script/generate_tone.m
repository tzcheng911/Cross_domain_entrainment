function [pure_tone, fs] = generate_tone(f0, fs, Dur, IOI, rampDur, win_choice)
    % create the sin wave
    nt = 0:1/fs:Dur-1/fs;
    N = length(nt);
    dur_unramp = cos(2*pi*f0*nt);

    % ramp the beginning and end of one tone
    ramp = floor(rampDur *fs) - 1;
    
    % ramping window
    switch win_choice
        case 'linear'
             ramp_f = linspace(0, 1, ramp);
             ramp_b = linspace(1, 0, ramp);
             window = [ramp_f ones(1, N-2*ramp) ramp_b];
             dur_out = dur_unramp.* window;
        case 'hanning'
             window = hanning(2*ramp)';
             w1 = window(1:ceil((length(window))/2)); %use the first half of hanning function for onramp
             w2 = window(ceil((length(window))/2)+1:end); %use second half of hanning function of off ramp
             w_on_xt = [w1 ones(1,length(dur_unramp)-length(w1))];
             w_off_xt = [ones(1,length(dur_unramp)-length(w2)) w2];
             dur_out = dur_unramp.*w_on_xt.*w_off_xt; 
    end
    pure_tone = dur_out;
    fs = fs;
end