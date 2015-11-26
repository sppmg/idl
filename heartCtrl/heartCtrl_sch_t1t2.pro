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
;count_total=fix(total([143,158,176,200,231,273]) ) ; include last number
;t0_start=0.42
;t0_end=0.22
;t0=dindgen(count_total)*((t0_end-t0_start)/double(count_total-1))+t0_start
;double([0.42,0.38,0.34,0.3,0.26,0.22])
;dt=MAKE_ARRAY(count_total, /double, VALUE = 3)
;double([])
;count=MAKE_ARRAY(count_total, /double, VALUE = 1)
;count=fix([143,158,176,200,231,273])	;count of pacing,lose first pacing



t0=double([0.42,0.38,0.34,0.3,0.26,0.22,0.20,.18])
dt=dblarr(size(t0, /N_ELEMENTS))+2.
count=round(60/t0)
	;count of pacing,lose first pacing

start_time=10.
debug=0
;=====================================
;if ((findfile(config))[0] ne "") then @config
;a=3&b=6&c=10&print,dindgen(c+1)*((b-a)/double(c))+a
len_t0=(size(t0))[1]
len_dt=(size(dt))[1]
len_count=(size(count))[1]
if ( len_t0 eq len_dt ) || ( len_dt eq len_count ) then begin
	len=len_t0
endif else begin
	print,"error:difference length t0,dt,count"
	retall
endelse

dt_old=0.
t0_old=0.
;================
print,"total ",total(count*t0)," mins"

;================

sign=1d
;t_start=double(systime(1))
;mark time
timer_count = long64(0)
timer_freq = long64(0)
timer, timer_count, timer_freq
t_start = timer_count


i=0			;part of exp == index of dt
pacingtime=0d
peak=dblarr(2)
while i lt len do begin

	for j=1,count(i) do begin

	;----- start t1t2 -----
	sign=1d
	if dt(i) ne 0. then begin

		pre = dblarr(100000)
		k=0L

		;------------ get peak -----------
		timer, timer_count, timer_freq
		t_read_start = timer_count

		cond = (t0(i)-dt(i)*1e-3 )*0.8		; old condition was "t0(i)*0.9"
		while t(t_read_start) lt cond do begin
			DAQ2IDL = DAQCtrl(AODevID, AIDevID, OP, Output, Input)
			pre[k] = Input[0]
			;print,pre[k]
			wait, 0.0009
			++k
		endwhile
		peak[1] = max(smooth(pre[0:k-1],10))

		if debug eq 1 then begin
			peak[0]=1.
			peak[1]=0.
		endif
		case 1 of
			;(abs(peak[1]-peak[0]) lt 0.0005):sign=0d
			(peak[1] gt peak[0]):sign=1d
			(peak[1] lt peak[0]):sign=-1d
			else :sign=0
		endcase
		if sign eq 0 then begin
			print,"dt=0"
		endif
		;print,"peak[0]=",peak[0]," / peak[1]=",peak[1],sign
		peak[0]=peak[1]

	endif

		;----- next pacing time ------
		pacingtime=pacingtime+t0(i)+sign*abs(dt(i)*0.001)

		while t(t_start) lt pacingtime  do begin	; wait to pacing time
			;print,(pacingtime-t(t_start))
			diff_time=pacingtime-t(t_start)
			;if diff_time gt 0.1 then wait,diff_time*0.5
		endwhile
		if debug ne 1 then begin	; pace
			Output[0] = 10
			DAQ2IDL = DAQCtrl(AODevID, AIDevID, OP, Output, Input)
			;wait, 0.001 ; <-from mi
			Output[0] = 0
			DAQ2IDL = DAQCtrl(AODevID, AIDevID, OP, Output, Input)
		endif

		;print,"out ",t(t_start),"/",diff_time,"/",t0(i),"/",sign*abs(dt(i)*0.001),"/",t0(i)+sign*abs(dt(i)*0.001)
		if dt(i) ne dt_old or t0(i) ne t0_old then begin
			dt_old=dt(i)
			t0_old=t0(i)
			print ,"t0=",t0(i)," ms / ","dt=",dt(i)," ms in ",t(t_start),"/",t(t_start)+start_time
		endif
		;print,"out ",t0(i),"/",sign*abs(dt(i)*0.001),"/",t0(i)+sign*abs(dt(i)*0.001)
		;output
	endfor
	++i
endwhile

;============= end ===============
OP = fix(3) ;Close Task
DAQ2IDL = DAQCtrl(AODevID, AIDevID, OP, Output, Input)
print, "All Done!"
end		;this pro file
