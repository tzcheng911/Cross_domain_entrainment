close all
clear all
cd('/Users/tzu-hancheng/Google_Drive/Research/cross_domain_entrainment/Vowel_length/pilot')

%% subject information
subDetails.Name = input('Subject Number (e.g., s01): ','s');
results_dir = fullfile(pwd,subDetails.Name);
    if ~exist(results_dir,'dir'), mkdir(results_dir), end
fnametemp = fullfile(results_dir,'temp_block_bar.mat');
counter=1;
fname = fullfile(results_dir,[subDetails.Name,num2str(counter),'test.mat']);
    while exist(fname,'file')
          counter=counter+1;
          fname = fullfile(results_dir,[subDetails.Name,num2str(counter),'test.mat']);
    end

HideCursor;
FlushEvents;
echo off    
iscreen = []; 
try 
    
%% window setting
AssertOpenGL;
    if isempty(iscreen)
        iscreen = max(Screen('Screens'));
    end
    [wPtr, rect] = Screen('OpenWindow', iscreen);
    w = wPtr;
    wRect = rect;

    resolutions = Screen('Resolution', iscreen);
    video.fps = Screen('FrameRate',wPtr); 
    slack = Screen('GetFlipInterval', wPtr)/2;
    IPF = Screen('GetFlipInterval', wPtr); 

    mx = 2560 ;      % mx = monitor width
    my = 1600 ;     % my = monitor height
    
    Screen('BlendFunction', wPtr, GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
    Screen('Preference', 'TextAlphaBlending', 1);
    
    syncToVBL = 1; 
    if syncToVBL > 0
        asyncflag = 0;
    else 
        asyncflag = 2;
    end

    dontclear = 1; 
    doclear = 0; 
    
%% text property
fontsize = 20;
font ='Helvetica';
KbName('UnifyKeyNames');
EscapeKey = KbName('Escape');
Space=KbName('Space');

%% set color
gray=[128 128 128];

%% set time
fixDuration = 0.5;
ITI = 1+rand*0.5;

%% factors
VL = [1:8]; % correspond to 8 vowel lengths
numTrials= 30; % number of trials per subcondition
vowel = {'Lap','Lab'};

%% core matrix
ntrial = 1;
output_result = [];  
    for i=1:length(VL)
        for m = 1:numTrials
        output_result(ntrial,1) = VL(i);
        output_result(ntrial,2) = 0; % RT
        output_result(ntrial,3) = 0; % Accuracy
        ntrial = ntrial + 1;
        end
    end    
    clear ntrial;
result = output_result(randperm(size(output_result,1)),:);

%% strating window
Screen('TextFont',wPtr, char(font));
Screen('TextSize', wPtr , fontsize);
%Screen('DrawText',wPtr,'Press Space key when you are ready to start.',mx/3,my/2, [0 0 0]);
DrawFormattedText(wPtr, 'Press Space key when you are ready to start.','center','center', [0 0 0],[],[],[],2);
Screen('Flip', wPtr);
KbWait;

%% Trial loop
nTrials = size(result,1);
for ntrial = 1:nTrials
    
% draw fixation
Screen('FillRect',wPtr,gray);
Screen('TextFont',wPtr, char(font));
Screen('TextSize', wPtr ,fontsize);
Screen('DrawText', wPtr, '+' ,mx/2-6 ,my/2-11 ,[0 0 0] ,[],[],[]);
t1=Screen('Flip', wPtr);
  
% draw preparation
Screen('FillRect',wPtr,gray);
Screen('DrawDots', wPtr, [mx/2;my/2] ,5,[0 0 0],[0,0],1);
t2=Screen('Flip', wPtr,t1+fixDuration-slack); 

% audio stimuli
nVL = num2str(result(ntrial,1));

[y,fs] = audioread(strcat(vowel{2},'_',nVL, '.wav'));
p = audioplayer(y,fs);
audio_start = GetSecs;
play(p)

% draw response reminder
Screen('FillRect',wPtr,gray);
Screen('TextSize', wPtr ,50);
DrawFormattedText(wPtr, strcat('F: ',vowel{1},',','J: ',vowel{2}),'center','center', [0 0 0],[],[],[],2);
%Screen('DrawText',wPtr,'F          J',mx/2.5,my/2, [0 0 0]);
%Screen('TextSize', wPtr ,14);
%Screen('DrawText',wPtr,'/ba/                                   /pa/',mx/2.5,my/1.8, [0 0 0]);
t3=Screen('Flip', wPtr,audio_start+numel(y)/fs-slack); 

%button = [0 0 0];
keytime = GetSecs;

            RT=0; 
% Keyboard version
%     [secs, aaa, deltaSecs] = KbWait([],2);
%     [keyIsDown, t1, aaa] = KbCheck;
%     if keyIsDown == 1
%         RT = GetSecs - keytime;
%     aaa = find(aaa, 1);
%      if aaa == 9 
%         result(ntrial,5)=100;
%         result(ntrial,6)=RT;
%      elseif aaa == 13 
%         result(ntrial,5)=200;
%         result(ntrial,6)=RT;
%      end  
%     end
    
% Keyboard version with key locked
    [secs, aaa, deltaSecs] = KbWait([],2);
    aaa = find(aaa, 1);
    while  aaa ~= 9 && aaa ~= 13
        [secs, aaa, deltaSecs] = KbWait([],2);
        aaa = find(aaa, 1);
        if  aaa == 9 || aaa == 13
            break
        end
    end
    
    [keyIsDown, t1, aaa] = KbCheck;
    aaa = find(aaa, 1);
    if keyIsDown == 1
        RT = GetSecs - keytime;
     if aaa == 9 % F key
        result(ntrial,3)=100;
        result(ntrial,2)=RT;
     elseif aaa == 13 % J key
        result(ntrial,3)=200;
        result(ntrial,2)=RT;
     end  
    end
     
 Screen('FillRect',wPtr,gray);
 t4=Screen('Flip', wPtr,GetSecs-slack,doclear);
 t5=Screen('Flip', wPtr,t4+ITI-slack,doclear);

% draw response reminder
% if (result(ntrial,2) == 1 && result(ntrial,5) == 100) || (result(ntrial,2) == 6 && result(ntrial,5) == 200)
%     feedback = 'Correct';
% elseif result(ntrial,2) == 1 && result(ntrial,2) == 200
%     feedback = 'No, this one is shorter';
% elseif result(ntrial,2) == 6 && result(ntrial,2) == 100
%     feedback = 'No, this one is longer';
% end
 timerecord(ntrial,1)=t1;
 timerecord(ntrial,2)=t2;
 timerecord(ntrial,3)=audio_start;
 timerecord(ntrial,4)=t3;
 timerecord(ntrial,5)=t4;
 timerecord(ntrial,6)=t5;
 timerecord(ntrial,7)=keytime;
 timerecord(ntrial,8)=RT;
 
%% how many breaks
if mod(ntrial,3)==0 
    Screen('TextSize', wPtr ,fontsize);
    %Screen('DrawText',wPtr,'This session is over.',mx/2.5, my/2,[0,0,0]);
    DrawFormattedText(wPtr, 'This session is over.','center','center', [0 0 0],[],[],[],2);
    Screen('Flip', wPtr);
    KbWait;
    [keyIsDown, t1, keyCode] = KbCheck;
     if keyCode(EscapeKey)
        break;
     else
        keyCode(Space)
     end  
end
end

%% Experiment  End
    Screen('FillRect', wPtr, gray);            
    DrawFormattedText(wPtr, 'End','center','center', [0 0 0],[],[],[],2);
    Screen('Flip', wPtr,[],[],1);
    WaitSecs(1);
    Priority(0);
    Priority(0);
    Screen('CloseAll');
    FlushEvents;
    ShowCursor;
catch
    psychrethrow(psychlasterror);
end
%% draw the categorical plot - find the threshold
numTrials = 30;
for plot_VL = 1:length(VL)
    ind = find((result(:,1) == plot_VL) & (result(:,3) == 200)); % percent of reporting bead
    prop(plot_VL) = length(ind)/numTrials;
end
x = [1:8];
%[est,p] = fit_logistic(VL,prop);
figure;
plot(x,prop,'o')  % data

lb = input('lower bound:','s');
lb = str2num(lb);
hb = input('upper bound:','s');
hb = str2num(hb);

save('parameters','lb','hb')
%     for i = 1:size(result,1)
%         if (result(i,4) < 4 && result(i,5) == 100) || (result(i,4) > 3 && result(i,5) == 200)
%             result(i,3) = 1;
%         else 
%             result(i,3) = 0;
%         end
%     end
% corr_index = find(result(:,7)==1);
% accuracy = length(corr_index)/length(result);
% clear y fs p nint ndelay begin_cIOI feedback
save(fname,'subDetails','result','timerecord');

