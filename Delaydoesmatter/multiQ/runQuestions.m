function [answer] = runQuestions(instrfile,expt,thename)
%% SC Creel, 2.11.2010
% This is a frame function for the inputdlg function.
% It takes two arguments:
%    -instrfile (the file of questions to be asked and formatting crap)
%    -expt (the experiment for which the questionnaire is about, to tell it
%    what file to attach the data to)
%    -also EXPECTS the global variable sname!!
% I'm using it to provide an input dialog for questionnaires
% When completed, this function will store basic question info with that
% subject's demodata (at the end of the file)
% see help inputdlg for more info

global sname

    name='Final Questions before you finish...';

    if nargin == 0

        prompt={'What is your name:','What is your quest:','What is your favourite colour:'};
        numlines=[1 30;3 100;3 100];
        defaultanswer={'Lancelot','To find the Holy Hand Grenade','blue no yellow'};
        answer=inputdlg(prompt,name,numlines,defaultanswer,'on');
        expt='holygrail';
        
    else
        % read in list
        fileID=fopen(instrfile);
        instrArray=textscan(fileID,'%s%n%n%s','CommentStyle','%','Delimiter','\t');
        fclose(fileID);
        prompt=instrArray{1};
        numlines=[instrArray{2} instrArray{3}];
        defaultanswer=instrArray{4};
        answer=inputdlg(prompt,name,numlines,defaultanswer,'on');
        
    end
    
    if nargin<3
        thename=sname;
    end
    
    datafid=fopen([expt 'Q'],'at'); % 'at' is append, text mode

    rowz=size(answer);

    
    for a=1:rowz(1)
        %fprintf(datafid,[expt '\t' sname '\t' char(instrArray{1}(a)) '\t' char(answer(a)) '\n']);
      fprintf(datafid,[expt '\t' thename '\t' char(instrArray{1}(a)) '\t' char(answer(a)) '\n']);
  
    end
    
    fclose(datafid)
    
    answer=sname
    
    % fprintf to an experiment file
    % store answer somewhere
    % print out answer to demodata file???
    
end











































