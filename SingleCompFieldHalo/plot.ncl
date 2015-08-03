;************************************
;
; plot.ncl
;
;************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl" 
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl" 
;************************************
begin
;************************************
 in = addfile("temp.nc","r")
 T=in->temperature
 
 wks = gsn_open_wks("x11","temp")  ; open a ncgm file
 res=True

 res@cnFillOn             = True     ; turn on color fill
 res@cnLineLabelsOn       = False    ; turns off contour line labels
 res@cnInfoLabelOn        = False    ; turns off contour info label

 res@tiXAxisString = "x"                    ; x-axis title
 res@tiYAxisString = "y"                    ; y-axis title
 res@tiMainString  = "temperature"          ; main title

 res@gsnDraw              = False    ; do not draw picture
 res@gsnFrame             = False    ; do not advance frame

 plot = gsn_csm_contour(wks,T,res)                ; create plot
 draw(plot)                                   ; draw plot
 frame(wks)                                   ; advance frame
;************************************
 
end


