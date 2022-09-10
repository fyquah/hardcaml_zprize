add_subdirectory(host)

# Here is not-so-standard element.
# The target installation command has to exist
# and has to install all linkable targets 
# into `.` (current) directory for cargo to find it.  
install(TARGETS zprize_msm_fpga DESTINATION .)
