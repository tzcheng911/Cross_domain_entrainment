clear
close all
clc
music_training = zeros(48,1);
for sub = [1:23,25:27,29:48];
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
    training = regexp(MusicBackground([14,15,21,22,28,29]),'\d+(\.)?(\d+)?','match');
    n = 1;
    for i = 1:2:5
        if isempty(training{i})
            start (n) = 0 ;
            finish (n) = 0;
        else
        start (n) = str2double(training{i});
        finish (n) = str2double(training{i+1});
        n = n+1;
        end
    end
    traning_total = sum(finish)-sum(start);
    music_training (sub,1) =  traning_total;
    clear n start finish training i 
end

