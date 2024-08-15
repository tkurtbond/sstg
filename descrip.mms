!+++
! Make an archival backup with today's date.
!---
zipit : 
	today = f$cvtime (,, "DATE")
        note = "$(NOTE)"
        set def [-]
	zipfile = "[.project_backups]sstg_" + today
        if note .nes. "" then zipfile = zipfile + "_" + note
        zipfile = zipfile + ".zip"
        write sys$output "Zipfile is ", zipfile
	! It's not really accurate unless we start from scratch.
	if f$search (zipfile) .nes. "" then delete 'zipfile';*/log
	zip -r 'zipfile' sstg.dir -x *.dsc *.key *.obj *.exe *.hlb *.lis *.map *.log
        write sys$output "Done zipiting!"

