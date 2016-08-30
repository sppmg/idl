; for proportional control
pro timer, count, freq
	DLL = "C:\data\brian\prog\idl\Timer.dll"
	Timer = call_external(DLL, "Timer", count, freq, /i_value)
end
pro add_slider,name,max_val,min_val,init_val,text
	base = WIDGET_BASE()
	name = widget_sLIDER(base,title=text,maximum=max_val,minimum=min_val,value=init_val)
	Widget_Control, /REALIZE, base
end

function t,ti
	timer_count = long64(0)
	timer_freq = long64(0)
	timer, timer_count, timer_freq
	;if ti le 0 then b
	return,(timer_count - ti)*1.0/timer_freq
	;return , double(systime(1))-double(ti)
end

FUNCTION DAQCtrl, AODevID, AIDevID, OP, Output, Input
	AODevID = byte(AODevID)
	AODevID = long(AODevID)
	AOIDLength = N_ELEMENTS(AODevID)

	AIDevID = byte(AIDevID)
	AIDevID = long(AIDevID)
	AIIDLength = N_ELEMENTS(AIDevID)

	DLL = "C:\data\brian\prog\idl\DAQCtrl.dll"

	DAQCtrl = call_external(DLL, "DAQCtrl_multiChan", $
	AODevID, AOIDLength, AIDevID, AIIDLength, Output, OP, Input, /i_value)
	RETURN, Input
END

;function pacing,t0,gain,count,len
;			AODevID = "Dev1/ao0:0"
;			AIDevID = "Dev1/ai0:1"
;			Output = dblarr(1)
;			Input = dblarr(1)
;			OP = fix(1)
;			DAQ2IDL = DAQCtrl(AODevID, AIDevID, OP, Output, Input)
;OP = fix(2) ;Start Task
;
;	return ,t_start
;end

;add_slider,ui_t0,65,20,40,'t0/10'
;add_slider,ui_gain,40,0,0,'gain*10'

;=========== program setting =========
;;;;; setup DAQ;;;;;;
AODevID = "Dev4/ao0:0"
AIDevID = "Dev4/ai0:1"
Output = dblarr(1)
Input = dblarr(1)
OP = fix(1) ;Create Task
DAQ2IDL = DAQCtrl(AODevID, AIDevID, OP, Output, Input)
OP = fix(2) ;Start Task

DAQ2IDL = DAQCtrl(AODevID, AIDevID, OP, Output, Input)
;============= user setting ==========
;t0=double([0.30,0.25,0.23,   0.23,   0.23])
;gain=double([0,0,0,   5,  0])
;count=fix([50,50,154,    260,   782])	;count of pacing,lose first pacing

;t0=double([0.42,0.38,0.34,0.3,0.26,0.22,0.20,.18])

; note :gain(i)*1e-4 , eg. 280
t0=double([0.26,0.22,0.20,.18])
gain=dblarr(size(t0, /N_ELEMENTS))+ 280
count=round(60/t0)

start_time=10.
debug=0
;=====================================
;if ((findfile(config))[0] ne "") then @config

;len_t0=(size(t0))[1]
;len_gain=(size(gain))[1]
;len_count=(size(count))[1]
;if ( len_t0 eq len_gain ) || ( len_gain eq len_count ) then begin
;	len=len_t0
;endif else begin
;	print,"error:difference length t0,gain,count"
;	retall
;endelse

gain_old=0.
t0_old=0.
;================
;print,"total ",total(count*t0)," mins"

;================

sign=1d
;t_start=double(systime(1))
;mark time
timer_count = long64(0)
timer_freq = long64(0)
timer, timer_count, timer_freq
t_start = timer_count


i=0			;part of exp == index of gain
pacingtime=0d
peak=dblarr(2)
pre = dblarr(5e5)
;while 1 do begin
len= size(gain, /N_ELEMENTS)
while i lt len do begin
	wait,1e-3

	dp =0    ; pressure differ
	for j=1,count(i) do begin
	;----- start control pacing -----
	sign=1d
	if gain(i) ne 0. then begin
		k=0L
		;------------ get peak -----------
		timer, timer_count, timer_freq
		t_read_start = timer_count

		cond = t0(i)*0.8		; old condition was "t0(i)*0.9"
		while t(t_read_start) lt cond do begin
			DAQ2IDL = DAQCtrl(AODevID, AIDevID, OP, Output, Input)
			pre[k] = Input[0]
			;print,pre[k]
			wait, 0.0009
			++k
		endwhile
		sl=ceil(k/cond*0.01)
		peak[1] = max(smooth(pre[0:k-1],sl))
		dp=(peak[1]-peak[0])
		peak[0]=peak[1]

	endif

	;----- next pacing time ------
	pacingtime=pacingtime+t0(i)+ gain(i)*dp

	while t(t_start) lt pacingtime  do begin	; wait to pacing time
		;print,(pacingtime-t(t_start))
		diff_time=pacingtime-t(t_start)
		;if diff_time gt 0.1 then wait,diff_time*0.5
	endwhile
	if debug ne 1 then begin	; pace
		Output[0] = 10
		DAQ2IDL = DAQCtrl(AODevID, AIDevID, OP, Output, Input)
		wait, 0.001 ; <-from mi
		Output[0] = 0
		DAQ2IDL = DAQCtrl(AODevID, AIDevID, OP, Output, Input)
	endif

	;print,"out ",t(t_start),"/",diff_time,"/",t0(i),"/",sign*abs(gain(i)*0.001),"/",t0(i)+sign*abs(gain(i)*0.001)
	if gain(i) ne gain_old or t0(i) ne t0_old then begin
		gain_old=gain(i)
		t0_old=t0(i)
		print ,"t0=",t0(i)," ms / ","gain=",gain(i)," ms in ",t(t_start),"/",t(t_start)+start_time
	endif
	;print,"out ",t0(i),"/",sign*abs(gain(i)*0.001),"/",t0(i)+sign*abs(gain(i)*0.001)
	;output
	endfor
	++i
endwhile

;============= end ===============
OP = fix(3) ;Close Task
DAQ2IDL = DAQCtrl(AODevID, AIDevID, OP, Output, Input)
print, "All Done!"
end		;this pro file
