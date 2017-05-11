; ref xmanager in doc

; event = struct(id, top, handler, select ... ) , top = all top
pro btn_cb, event   ; button CallBack function
	;help,event,/structure
	
	type=widget_info(event.id,/type) ; 0=base, 1=button, 2=slider, 3=text .... see widget_info doc
	
	if type eq 1 then begin
		parent=widget_info(event.id,/parent)
		member=widget_info(parent, /all_children)			
		members_end=n_elements(member)-1
	
		for i=0,members_end do begin
			;print, "member=", member[i]   ; some debug code
			;wait ,1e-3
			
			case widget_info(member[i],/type) of 
				1:	begin ;button
						widget_control, event.id, get_value=v
						case v of
							'+' :chop=1
							'-' :chop=-1
						endcase
					end
				
				2:	begin ; slider(event.value is changed value)
						slider_id=member[i]
						widget_control, member[i], get_value=slider_val
					end
				
				3:	begin ; text
						widget_control, member[i], get_value = step
						step=long(step)
					end
			endcase
		endfor
		widget_control, slider_id, set_value = (slider_val+chop*step)
	endif
end  ; btn_cb


;============= GUI setting ===========
gui_base=widget_base(column=1)

; a widget group for t0
gui_cg_t0=widget_base(gui_base, row=1,title="t0") ; control group
gui_sld_t0=widget_slider(gui_cg_t0, title="t0(s) x 100", minimum=15, maximum=65, value=40)
gui_btn_t0m=widget_button(gui_cg_t0, value="-")
gui_txt_t0_step=widget_text(gui_cg_t0, /editable, /no_newline, xsize=5, value='5') ; use '' not ""
gui_btn_t0p=widget_button(gui_cg_t0, value="+")

; another group(e.g. dt)
gui_cg_dt=widget_base(gui_base, row=1,title="dt") ; control group
gui_sld_dt=widget_slider(gui_cg_dt, title="dt (ms)", minimum=0, maximum=20, value=0)
gui_btn_dtm=widget_button(gui_cg_dt, value="-")
gui_txt_dt_step=widget_text(gui_cg_dt, /editable, /no_newline, xsize=5, value='1') ; use '' not ""
gui_btn_dtp=widget_button(gui_cg_dt, value="+")


widget_control, gui_base, /realize		; display on screen


; Register the widget with the XMANAGER, leaving the IDL command

XMANAGER, 'name', gui_base, event_handler='btn_cb', /no_block
; "name" is a name for xmanager.(maybe for manager)
; gui_base is ID for event.
; event_handler set callback handler routine, It execute when event trigging (e.g. click button).
; if don't set event_handler, it will find 'name'+'_event' pro


; test event in busy loop
t=10e3    ; about 10 s
while t ge 0 do begin
	ev=widget_event(gui_base,/nowait) ; answer event
	
	wait,1e-3  ; wait 1 ms
	widget_control, gui_sld_t0, get_value = v
	print,"t=",t,"    slider value=",v
	
	t--
endwhile

end
 
