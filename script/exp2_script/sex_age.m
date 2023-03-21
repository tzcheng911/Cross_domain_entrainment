clear 
close all
clc
for sub = [1:27,29:48]
    str='s0';
    if sub>=10
       str='s';
    end
    route='/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp2/tidy_data_first48/';
    % route='/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp2/outliers/';

    data_route = [route str sprintf('%d',sub)];
    cd (data_route);
    data = [str sprintf('%d questionnaires',sub)];
    load (data);
    tempsex = split(basicdialog(2));
    sex{sub,1} = tempsex(2);
    tempage = split(basicdialog(13));
    age(sub,1) = str2num(tempage{2});
    clear basicdialog
end

