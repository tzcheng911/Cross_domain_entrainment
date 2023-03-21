function [phi, P, C] = timekeeperMJ(IOI, W_phi, W_p, phi0, P0)
% McAuley General Timekeeper
% IOI - a vector of IOI values (presumably in seconds)
% W_phi - phase reset value, scalar from 0 to 1
% W_p - period reset value, scalar from 0 to 1
% phi0 - initial phase of timekeeper, default to 0
% P0 - initial period of timekeeper, default to value of first IOI


if nargin < 5
    P0 = IOI(1); %By default M&J set P0 to equal the initial interval,
end              %ie., they do not care about entrainment....
if nargin < 4;
    phi0 = 0; %By default M&J set intial phase to 0 (right on time);
end

phi = nan(size(IOI));
phi(1) = phi0;
P = nan(size(IOI));
P(1) = P0;
C = nan(size(IOI));
rC = nan(size(IOI));


%Phase and Period Correction
for i = 1:length(IOI)
    [c, rc] = temporalContrast(phi(i), IOI(i), P(i));
    C(i) = c;
    rC(i) = rc;
    %Linear Phase Correction
    phi(i+1) = (1 - W_phi) * c;
    %Period Correction
    P(i+1) = (1 + W_p * c) * P(i);
end

% %% Plot Timekeeper Performance 
% %against IOIs
% figure;
% subplot(3,1,1);
% stem(IOI); hold on; plot(P);
% xlabel('Timekeeper Period against IOI');
% ylabel('Period');
% title(['General Timekeeper w/ W_p_h_i = ' num2str(W_phi) ...
%     ', W_P = ' num2str(W_p) ])
% hold off; legend('IOI', 'P');grid on
% 
% subplot(3,1,2); hold on;
% plot(phi); plot(C);
% plot(phi, 'x', 'Color', 'blue');plot(C, 'o', 'Color', 'red');
% legend('Phase', 'C')
% xlabel('Timekeeper Relative Phase against C')
% ylabel('Relative Phase')
% hold off; grid on;
% 
% subplot(3,1,3);
% IOI_onset = cumsum(IOI);
% TK_onset = cumsum(IOI) - IOI.*(C);
% hold on
% for ii = 1:length(IOI)
%     line( [IOI_onset(ii) IOI_onset(ii)], [0 1], 'Color', 'blue', 'LineWidth', 2)
%     line( [TK_onset(ii) TK_onset(ii)], [1 2], 'Color',...
%         [0+abs(C(ii)^.5) 0 1-abs(C(ii)^.5)],  'LineWidth', 2)
% end
% xlabel('Time (s)'); ylabel('Onset Times')
% ylim([0 2]); set(gca, 'YTick', [0.5 1.5]);
% set(gca, 'YTickLabel', ({'IOI onsets', 'P onsets'}));
% grid on;
% hold off


