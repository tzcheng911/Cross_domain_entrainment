clear all
close all
clc
for sub=63
    str='s0';
    if sub>=10
       str='s';
    end
    data_route='/Users/tzu-hancheng/Google_Drive/Academia/LASR/model_distinguish_task_delay/data_analysis';
    cd (data_route);
    data = [str sprintf('%d4',sub)];
    load (data);
    sex{sub,1} = basicdialog(2);
    age{sub,1} = basicdialog(13);
    clear basicdialog
end

