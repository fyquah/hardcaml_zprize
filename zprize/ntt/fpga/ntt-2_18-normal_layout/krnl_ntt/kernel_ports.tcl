set core [ipx::current_core]

ipx::associate_bus_interfaces -busif controller_to_compute_phase_1 -clock ap_clk $core
ipx::associate_bus_interfaces -busif controller_to_compute_phase_2 -clock ap_clk $core
ipx::associate_bus_interfaces -busif compute_to_controller -clock ap_clk $core
