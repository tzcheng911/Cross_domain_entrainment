clear 
close all
clc
numTrials = 30;
VOT = [8.845,15.279,21.039,26.821,31.357,36.731];
prop1 = [0.015 0.019 0.8 0.89 0.9 1];
prop2 = [0.018 0.029 0.021 0.85 0.91 1];
prop3 = [0.01 0.015 0.017 0.2 0.92 0.95];
x = VOT;
[est1,p1] = fit_logistic(x,prop1);
[est2,p2] = fit_logistic(x,prop2);
[est3,p3] = fit_logistic(x,prop3);
test = [prop1;prop2;prop3];
estall = [est1;est2;est3];
figure;
plot(x,prop1,'o','Color', [200/255 200/255 10/255])
hold on
plot(x,prop2,'ko')
hold on
plot(x,prop3, 'o','Color', [0.9100 0.4100 0.1700]);
hold on 
plot(x,est1, 'Color', [200/255 200/255 10/255],'LineWidth',2) 
hold on
plot(x,est2,'k-','LineWidth',2) 
hold on
plot(x,est3,'Color', [0.9100 0.4100 0.1700],'LineWidth',2) 
legend('Early','On Time','Late')
set(gca,'FontSize',1)




