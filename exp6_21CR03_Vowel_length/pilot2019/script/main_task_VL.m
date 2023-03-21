%%
close all
clear 
load('parameters.mat')
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
VL = [1:6]; % correspond to 6 VLs
begin_time = [1,2,3]; % correspond to early(1), ontime(2), late(3) beginning
numTrials= 6; %number of trials per subcondition
all_VLs = {'Lab_1','Lab_2','Lab_3','Lab_4','Lab_5','Lab_6','Lab_7','Lab_8'};
file_VLs = {all_VLs{lb-2:lb},all_VLs{hb:hb+2}};
file_begin = {'early','ontime','late'};
vowel = {'Lap','Lab'};

%% core matrix
ntrial = 1;
output_result = [];  
    for i=1:length(VL) 
        for j=1:length(begin_time) 
            for sex = 1:2
                for m = 1:numTrials
                    output_result(ntrial,1) = VL(i);
                    output_result(ntrial,2) = begin_time(j);
                    output_result(ntrial,3) = sex;                
                    output_result(ntrial,4) = 0;
                    output_result(ntrial,5) = 0;
                    output_result(ntrial,6) = 0;
                    ntrial = ntrial + 1; 
                end
            end
                
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
nVL = file_VLs{result(ntrial,1)};
nbegin_time = file_begin{result(ntrial,2)};
nsex = num2str(result(ntrial,3));

[y,fs] = audioread([nbegin_time '_' nVL '.wav']);
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
        RT = GetSecs - audio_start;
     if aaa == 9 % F
        result(ntrial,4)=100; 
        result(ntrial,5)=RT;
        result(ntrial,6)=aaa;
     elseif aaa == 13 % J
        result(ntrial,4)=200;
        result(ntrial,5)=RT;
        result(ntrial,6)=aaa;
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

%% questionaires
basicdialog = basicdialog;
LanguageBackgroundBDS = LanguageBackgroundBDS;
save(fname,'subDetails','result','file_VLs','timerecord','basicdialog','LanguageBackgroundBDS');