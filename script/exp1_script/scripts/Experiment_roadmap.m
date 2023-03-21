%% Experiment Roadmap
%% make all stimuli first 
    use garageband to create stimuli for each condtion
%% make a txt file for storing the stimuli list
    length,onset,conditions
%% load all trial info (stroed in structs)
%% Create data file (txt is better than mat because no need to convert)
%% Start the experiment
    psychtoolbox
    Intro Screen
%% Trial loop
load trial audio
present fixation
play audio
wait for response
write trial info/response to .txt
save the file

close

%% some sources 
http://peterscarfe.com/gaborarraydemo.html

%%
trials(1).stiname = 'stim1.wav';
trials(1).length = 34.1;

for trialNo = 1 : length(trials)
    stim = trials(trialNo).stimname

s1 = GetSecs;
s2 = GetSecs;
RT = s2 - s1;

soundsc(audio,fs);
ap = audioplayer(audio,fs);
ap.play;
