%% duplicate the results of main task, create a new mat file named by the subj number
music_training = zeros(19,1);
for sub=1:19
    str='s0';
    if sub>=10
       str='s';
    end
    data_route=['/Users/tzu-hancheng/Desktop/temp_for_water/Academia/LASR/Replicate_MJ/data/' str sprintf('%d',sub)];
    cd (data_route);
    data = [str sprintf('%d1test',sub)];
    load (data);
    training = regexp(MusicBackground([14,15,21,22,28,29]),'\d+(\.)?(\d+)?','match');
    n = 1;
    for i = 1:2:5
    start (n) = str2double(training{i});
    finish (n) = str2double(training{i+1});
    n = n+1;
    end
    traning_total = sum(finish)-sum(start);
    music_training (sub,1) =  traning_total;
    clear n start finish training i
end

