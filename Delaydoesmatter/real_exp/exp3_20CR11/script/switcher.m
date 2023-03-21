cd('/Users/t.z.cheng/Google Drive/Research/Delaydoesmatter/real_exp/exp2')
nsub = 1;
counterbalance = repmat([1:12],1,4);
switch counterbalance(nsub)
    case 1
        delay2_LR
        delay4_LR
        delay8_LR
    case 2
        delay4_LR
        delay2_LR
        delay8_LR
    case 3
        delay4_LR
        delay8_LR
        delay2_LR
    case 4
        delay2_LR
        delay8_LR
        delay4_LR
    case 5
        delay8_LR
        delay2_LR
        delay4_LR
    case 6
        delay8_LR
        delay4_LR
        delay2_LR
    case 7
        delay2_RL
        delay4_RL
        delay8_RL
    case 8
        delay4_RL
        delay2_RL
        delay8_RL
    case 9
        delay4_RL
        delay8_RL
        delay2_RL
    case 10
        delay2_RL
        delay8_RL
        delay4_RL
    case 11
        delay8_RL
        delay2_RL
        delay4_RL
    case 12
        delay8_RL
        delay4_RL
        delay2_RL
end

basicdialog = basicdialog;
MusicBackground = MusicBackground;
save(strjoin({subDetails.Name,'questionnaires'}),'basicdialog','MusicBackground');