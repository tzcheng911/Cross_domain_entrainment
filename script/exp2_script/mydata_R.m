%% 
clear 
close all
clc
cd('/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp2/data_analysis')

%% Only get the shortest condition
begin_time = [5,8,9];
com_IOI = [1,2,3,4,5,6];
delay = '2';
sub = 1:48;

nsubj = length(sub);
ntrial = 216;
nbegin = length(begin_time);
ncomp = length(com_IOI);
ndelay = length(delay);
raw_mydata = num2cell(zeros(nsubj,ntrial,10));

for subj = 1:nsubj
    for del = 1:ndelay
    str = 's0';
    cond = delay(del);
    if sub(subj) >= 10
       str = 's';
    end
    data_route = '/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp2/data_analysis';
    cd (data_route);
    data = [str num2str(sub(subj)) cond 'test'];
    load (data);
    if result(1,2) ~= str2num(cond) || size(result,1) ~= 216 
       error('Wrong file.'); 
    end
    
    % mark the short trial as 1 and long trial as 0 in the eighth
% colume in matrix "result"
    for i = 1:size(result,1)
        if (result(i,5) == 100) % The trials considered as shorter
            result(i,8) = 1;
        else 
            result(i,8) = 0;
        end
    end
    subjind = [str num2str(sub(subj))];
    repsubj = repmat({subjind},ntrial,1);
    sub_data = [repsubj,(num2cell(1:ntrial))',num2cell(result)];
    raw_mydata(subj,:,:) = sub_data;
    end
end
%%    
mydata = reshape(raw_mydata,48*216,10);
T_mydata = cell2table(mydata);
writetable(T_mydata,'exp2_PPS_n48.csv')
