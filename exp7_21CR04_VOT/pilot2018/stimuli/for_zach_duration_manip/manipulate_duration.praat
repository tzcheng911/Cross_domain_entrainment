###################################
#
# DURATION MANIPULATION VIA PSOLA
# 
# Manipulates duration of an interval for a user-specified number of steps
# within a user-specified range.
#
# Requires a .wav file and corresponding TextGrid with a label "v"
# on Tier 1 for the interval that you want to be manipulated.
#
# May 28, 2015
# Questions? jessamyn.schertz@gmail.com
#
###################################



## USER INPUT #######################

filename$ = "pear_natural"

asp.min = 0.1
asp.max = 0.5
num.steps = 5

manip.label$ = "v"

###################################

## Read in files, get info from TextGrid
sound = Read from file... 'filename$'.wav
tg = Read from file... 'filename$'.TextGrid
num.int = Get number of intervals... 1
for int from 1 to num.int
	label$ = Get label of interval... 1 int
	if label$ == manip.label$

		# variable names are all "asp" because this was originally an aspiration manipulation...

		asp.start = Get start point... 1 int
		asp.end = Get end point... 1 int
		asp.dur = asp.end-asp.start
	endif
endfor
dur = Get total duration

## Figure out step increment
asp.step = (asp.max-asp.min)/(num.steps-1)

## Loop through steps
for step from 1 to num.steps
	select 'sound'
	asp = asp.min+(step-1)*asp.step
	new.dur = dur - (asp.dur-asp)
	manip = To Manipulation... 0.01 75 500
	dur.tier = Create DurationTier... anon 0 dur

	## The tiny differences below are a bit of a hack.
	## The idea is to get a single interval multiplied by a single relative-duration value.
	## But because of the way the "duration points" work, 
	## you can't switch directly from one value to another.
	## Instead, it will always do a linear interpolation between two points. 
	## In the example here, relative duration is at 1 (i.e. no change) until asp.start.
	## Then JUST after the start, relative duration is at the value to be manipulated.
	## Between 0 and 0.000001 s., the relative duration is linearly interpolated 
	## between 1 and the desired value. 
	##
	## This might be easier to see if you just uncomment out the "pause" line below.
	## It will stop and open up the duration tier for you to inspect.

	Add point... asp.start 1
	Add point... asp.start+0.0000001 asp/asp.dur
	Add point... asp.end-0.0000001 asp/asp.dur
	Add point... asp.end 1
	plus 'manip'
	Replace duration tier
	select 'manip'
	
	## Uncomment the following lines to see how the duration tier looks for each item:
	# View & Edit
	# pause Check out the duration tier
	# Close


	newsound = Get resynthesis (overlap-add)
	asp = asp*1000
	asp = round(asp)
	asp$ = "'asp'"
	filename.new$ = filename$ + "_" + asp$
	Rename... 'filename.new$'

	## Clean up
	select 'manip'
	plus 'dur.tier'
	Remove
endfor