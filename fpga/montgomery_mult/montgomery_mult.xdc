# Create a 250MHz clock.
create_clock -name clock -period 4.00 [get_ports clock]
# Required for getting timing reports on out_ot_context runs.
set_property HD.CLK_SRC BUFGCTRL_X0Y16 [get_ports clock]
