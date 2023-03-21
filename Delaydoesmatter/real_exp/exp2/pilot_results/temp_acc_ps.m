%% set up 
clear 
close all

load('zoe31test.mat')
begin_time = [5,8,9];
com_IOI = [1,2,3,4,5,6];

%% accuracy
for i = 1:size(result,1)
    if (result(i,4) < 4 && result(i,5) == 100) || (result(i,4) > 3 && result(i,5) == 200)
        result(i,8) = 1;
        else 
        result(i,8) = 0;
    end
end

for k = 1:length(begin_time)
    for l = 1:length(com_IOI)
        index = find ((result(:,3)==begin_time(k)) & (result(:,4)==com_IOI(l)));
        corr_index = find(result(index,8)==1);
        accuracy (k,l) = length(corr_index)/length(index);
    end 
end
mean(accuracy,1)
%% proportion short
for i = 1:size(result,1)
    if (result(i,5) == 100) % The trials considered as shorter
        result(i,8) = 1;
    else 
        result(i,8) = 0;
    end
end

for k = 1:length(begin_time)
    for l = 1:length(com_IOI)
        index = find ((result(:,3)==begin_time(k)) & (result(:,4)==com_IOI(l)));
        shorter_index = find(result(index,8)==1);
        ps (k,l) = length(shorter_index)/length(index);
    end 
end
mean(ps,2)