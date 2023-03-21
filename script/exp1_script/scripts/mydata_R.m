clear all
close all
clc
%% 
begin_time = [5,8,9];
com_IOI = [1,2,3,4,5,6];
delay = ['2';'4'];
sub = [3,4,6,7,8,10,12:19,21:23,25:32,34,36,37,39,40,41,47:63];
slsubs = [3,4,6:8,10,12:15,19,39:41,51,52,54:58,60:62];
lssubs = [16:18,21:23,25:32,34,36,37,47:50,53,59,63];
sdelay = num2cell(ones(216,1));
ldelay = num2cell(2*ones(216,1));
sldelay = {sdelay;ldelay};
lsdelay = {ldelay;sdelay};
nsubj = length(sub);
ntrial = 216;
nbegin = length(begin_time);
ncomp = length(com_IOI);
ndelay = length(delay);
raw_mydata = num2cell(zeros(nsubj,ndelay,ntrial,7));

for subj = 1:nsubj
    for del = 1:ndelay
    str = 's0';
    cond = delay(del);
    if sub(subj) >= 10
       str = 's';
    end
    data_route = '/Users/tzu-hancheng/Google_Drive/Academia/LASR/model_distinguish_task_delay/data_analysis';
    cd (data_route);
    data = [str num2str(sub(subj)) cond];
    load (data);
    if result(1,2) ~= str2num(cond) || size(result,1) ~= 216 
       error('Wrong file.'); 
    end
% mark the correct trial as 1 and incorrect trial as 0 in the eighth
% colume in matrix "result"
    for i = 1:size(result,1)
        if (result(i,4) < 4 && result(i,5) == 100) || (result(i,4) > 3 && result(i,5) == 200)
            result(i,8) = 1; % correct
        else 
            result(i,8) = 0; % incorrect
        end
    end
    subjind = [str num2str(sub(subj))];
    repsubj = repmat({subjind},ntrial,1);
    ls = ismember(sub(subj),lssubs);
    sub_data = [repsubj,(num2cell(1:ntrial))',num2cell(result(:,[2,3,4,8]))];
    raw_mydata(subj,del,:,1:6) = sub_data;
    
% There are two ways coding the present order
%         if ismember(sub(subj),slsubs)
%            raw_mydata(subj,del,:,7) = sldelay{del};
%         else
%            raw_mydata(subj,del,:,7) = lsdelay{del};
%         end

        if ismember(sub(subj),slsubs)
           raw_mydata(subj,del,:,7) = {1}; % 1 for sl
        else
           raw_mydata(subj,del,:,7) = {2}; % 2 for ls
        end
    end
end   
mydata = reshape(raw_mydata,48*2*216,7);

%% Modify the trial num (1-432) and present order (1: short, long; 2: long, short)
sort_mydata = sortrows(mydata,1);
subj_num = 48;
trial_num = 432;
new_mydata_original = num2cell(zeros(subj_num*trial_num,7));
for j = 1:48
    new_mydata_original ((((j-1)*trial_num+1):j*trial_num),:) = sortrows(sort_mydata((((j-1)*trial_num+1):j*trial_num),:),3);
    new_mydata = new_mydata_original;
    new_mydata ((((j-1)*trial_num+1):j*trial_num),2) = num2cell(1:trial_num);
end

new_mydata = new_mydata_original;
new_mydata(:,2) =  num2cell(repmat((1:trial_num)',48,1));
