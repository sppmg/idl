; ref xmanager in doc


pro btn_cb, event
	widget_control, event.id, get_value=uv
	if (uv eq "button") then begin
		print,"button clicked"
	endif
end


;============= GUI setting ===========
gui_base=widget_base(column=1)

gui_ctrl_group=widget_base(gui_base, row=1)
gui_sld_t0=widget_slider(gui_ctrl_group, title="t0(s) x 100", minimum=15, maximum=65, value=40)
gui_btn_t0p=widget_button(gui_ctrl_group, value="button")



widget_control, gui_base, /realize		; display on screen


; Register the widget with the XMANAGER, leaving the IDL command

XMANAGER, 'name', gui_base, event_handler='btn_cb', /NO_BLOCK
; "name" is a name for xmanager.(maybe for manager)
; gui_base is ID for event.
; event_handler set callback handler routine, It execute when event trigging (e.g. click button).
; if don't set event_handler, it will find 'name'+'_event' pro

end

