if { $argc != 1 } {
    puts "The script requires input top level module name."
    exit
}
set application [lindex $argv 0]

create_project -part xcvu9p-flga2104-3-e -in_memory 
add_files ${application}.v
add_files ${application}.xdc

set_property top ${application} [current_fileset]

synth_design -mode out_of_context
report_utilization -hierarchical -file post_synth_utilization.rpt
report_timing_summary -file post_synth_timing.rpt 
write_checkpoint post_synth_checkpoint.dcp -force
