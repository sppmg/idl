; for GUI proportional control
pro timer, count, freq
	DLL = "C:\data\brian\prog\idl\Timer.dll"
	Timer = call_external(DLL, "Timer", count, freq, /i_value)
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

;function pacing,t0,dt,count,len
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

; add_slider,ui_t0,65,20,40,'t0/10'
; add_slider,ui_dt,50,0,0,'gain*10'
; add_slider,ui_eps_gain,20,0,0,'eps gain'


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
;dt=double([0,0,0,   5,  0])
;count=fix([50,50,154,    260,   782])	;count of pacing,lose first pacing
start_time=10.
debug=0
eps_base=1e-4
;============= GUI setting ===========
gui_base=widget_base(column=1)
gui_sld_t0=widget_slider(gui_base, title="t0(s) x 100", minimum=15, maximum=65, value=40)
gui_sld_gain=widget_slider(gui_base, title="gain x 1e-4", minimum=0, maximum=500, value=0)
gui_sld_eps=widget_slider(gui_base, title="eps x 1e-4", minimum=0, maximum=500, value=0)
widget_control, gui_base, /realize		; display on screen
;=====================================
;if ((findfile(config))[0] ne "") then @config

;len_t0=(size(t0))[1]
;len_dt=(size(dt))[1]
;len_count=(size(count))[1]
;if ( len_t0 eq len_dt ) || ( len_dt eq len_count ) then begin
;	len=len_t0
;endif else begin
;	print,"error:difference length t0,dt,count"
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
while 1 do begin
	wait,1e-3
	dp = 0
	
	widget_control, gui_sld_t0, get_value=t0
	t0=t0*1e-2
	widget_control, gui_sld_gain, get_value=gain
	gain=gain*1e-4
	widget_control, gui_sld_eps, get_value=eps_gain
	eps=eps_base*eps_gain
	
	;----- start t1t2 -----
	sign=1d
	if gain ne 0. then begin

		pre = dblarr(1e5)
		k=0L

		;------------ get peak -----------
		timer, timer_count, timer_freq
		t_read_start = timer_count

		cond = t0*0.8		; old condition was "t0(i)*0.9"
		while t(t_read_start) lt cond do begin
			DAQ2IDL = DAQCtrl(AODevID, AIDevID, OP, Output, Input)
			pre[k] = Input[0]
			;print,pre[k]
			wait, 0.0009
			++k
		endwhile
		sl=ceil(k/cond*0.01)
		peak[1] = max(smooth(pre[0:k-1], sl))
		dp=(peak[1]-peak[0])
		peak[0]=peak[1]

	endif
	if  abs(dp) lt eps then dp=0



	;----- next pacing time ------
	pacingtime=pacingtime+t0(i)+ dp*gain

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
	if gain ne gain_old or t0 ne t0_old then begin
		gain_old=gain
		t0_old=t0
		print ,"t0=",t0," ms / ","gain=",gain," ms in ",t(t_start),"/",t(t_start)+start_time
	endif
	;print,"out ",t0(i),"/",sign*abs(gain(i)*0.001),"/",t0(i)+sign*abs(gain(i)*0.001)
	;output

endwhile

;============= end ===============
OP = fix(3) ;Close Task
DAQ2IDL = DAQCtrl(AODevID, AIDevID, OP, Output, Input)
print, "All Done!"
end		;this pro file
