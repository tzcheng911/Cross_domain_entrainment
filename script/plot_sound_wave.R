#### Visualize the sound waves
install.packages("tuneR")
library(tuneR)

## Visualize the official stimuli
path = '/Users/tzu-hanzoecheng/Documents/GitHub/Cross_domain_entrainment/'
EXP9_speech = 'exp9ab/stimuli/official_stimuli/speech_target_70db/'
EXP10_speech = 'exp9ab/stimuli/official_stimuli/'
EXP9_tone = 'exp9ab/stimuli/official_stimuli/avg_env_tone_target_85db/'
EXP10_tone = 'exp9ab/stimuli/official_stimuli/'

wav_file <- readWave(paste(path,EXP9_tone,'300ms_ontime_delay_2_avg_env_tone_1.wav',sep=""))
plot(wav_file,xlim=c(),ylim=c())

plot(wav_file)
