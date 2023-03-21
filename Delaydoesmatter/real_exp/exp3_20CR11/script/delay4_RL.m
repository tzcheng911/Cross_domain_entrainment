

%%
close all
clear 
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

    mx = 1280 ;      % mx = monitor width
    my = 800 ;     % my = monitor height
    
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
ITI = 1;

%% factors
std_IOI = [6];
delay_dur = [4]; 
begin_time = [5,8,9]; % correspond to early(1), ontime(2), late(3) beginning
comp_IOI = [1,2,3,4,5,6]; % correspond to 0.55(1), 0.57(2), 0.59(3), 0.61(4), 0.63(5), 0.65(6) cIOI
numTrials= 12; %number of trials per subcondition

%% core matrix
ntrial = 1;
output_result = [];  
    for i=1:length(std_IOI) 
        for j=1:length(delay_dur) 
            for k=1:length(begin_time)
                for l=1:length(comp_IOI)
                    for m = 1:numTrials
                    output_result(ntrial,1) = std_IOI(i);
                    output_result(ntrial,2) = delay_dur(j);
                    output_result(ntrial,3) = begin_time(k);
                    output_result(ntrial,4) = comp_IOI(l);
                    output_result(ntrial,5) = 0;
                    output_result(ntrial,6) = 0;
                    output_result(ntrial,7) = 0;
                    ntrial = ntrial + 1;
                    end
                end
            end
        end
    end    
    clear ntrial;
result = output_result(randperm(size(output_result,1)),:);

%% strating window
Screen('TextFont',wPtr, char(font));
Screen('TextSize', wPtr , fontsize);
Screen('DrawText',wPtr,'Press Space key when you are ready to start.',mx/3,my/2, [0 0 0]);
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
ndelay = num2str(result(ntrial,2));
nbegin_time = num2str(result(ntrial,3));
ncomp_IOI = num2str(result(ntrial,4));

[y,fs] = audioread(['delay_' ndelay '_' nbegin_time ncomp_IOI '.wav']);
p = audioplayer(y,fs);
audio_start = GetSecs;
play(p)

% draw response reminder
Screen('FillRect',wPtr,gray);
Screen('TextSize', wPtr ,50);
Screen('DrawText',wPtr,'F           J',mx/2.5,my/2, [0 0 0]);
Screen('TextSize', wPtr ,14);
Screen('DrawText',wPtr,'for longer                              for shorter',mx/2.5,my/1.8, [0 0 0]);
t3=Screen('Flip', wPtr,audio_start+numel(y)/fs-slack); 

keytime = GetSecs;

            RT=0; 
    
% Keyboard version with key locked
% Use KbName('KeyNamesOSX') to check the name of different key number on
% ios: F 9 (200), J 13 (100)
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
     if aaa == 9 
        result(ntrial,5)=200;
        result(ntrial,6)=RT;
        result(ntrial,7)=aaa;
     elseif aaa == 13 
        result(ntrial,5)=100;
        result(ntrial,6)=RT;
        result(ntrial,7)=aaa;
     end  
    end
     
% Mouse version
%             while ~any(button) %wait mouse response
%                 [mouse_x,mouse_y,button] = GetMouse;
%                 if any(button)
%                     RT = GetSecs - keytime;
%                     if button(1) && ~ button(2) 
%                        result(ntrial,5)=100;
%                        result(ntrial,6)=RT;
%                     elseif button(2) && ~ button(1)
%                        result(ntrial,5)=200;  
%                        result(ntrial,6)=RT;
%                     end
%                 end
%             end

 Screen('FillRect',wPtr,gray);
 t4=Screen('Flip', wPtr,GetSecs-slack,doclear);
 t5=Screen('Flip', wPtr,t4+ITI-slack,doclear);

 timerecord(ntrial,1)=t1;
 timerecord(ntrial,2)=t2;
 timerecord(ntrial,3)=audio_start;
 timerecord(ntrial,4)=t3;
 timerecord(ntrial,5)=t4;
 timerecord(ntrial,6)=t5;
 timerecord(ntrial,7)=keytime;
 timerecord(ntrial,8)=RT;
 
%% how many breaks
if mod(ntrial,54)==0 
    Screen('TextFont',wPtr, char(font));
    Screen('TextSize', wPtr ,fontsize);
    Screen('DrawText',wPtr,'Take a break! Press space button when you are ready.',mx/3, my/2,[0,0,0]);
    Screen('Flip', wPtr);
    KbWait;
    [keyIsDown, t1, keyCode] = KbCheck;
     if keyCode(EscapeKey)
        break;
     else
        keyCode(Space)
     end  
end

clear y fs p nint ndelay begin_cIOI
save(fname,'subDetails','result','timerecord');
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

%% save the files
save(fname,'subDetails','result','timerecord');