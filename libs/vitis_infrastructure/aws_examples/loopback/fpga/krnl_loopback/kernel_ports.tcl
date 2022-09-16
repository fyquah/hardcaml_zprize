set core [ipx::current_core]

ipx::associate_bus_interfaces -busif fpga_to_host -clock ap_clk $core
ipx::associate_bus_interfaces -busif host_to_fpga -clock ap_clk $core
ipx::associate_bus_interfaces -busif s_axi_control -clock ap_clk $core

set mem_map    [::ipx::add_memory_map -quiet "s_axi_control" $core]
set addr_block [::ipx::add_address_block -quiet "reg0" $mem_map]

set reg      [::ipx::add_register -quiet "foo" $addr_block]
  set_property address_offset 0x0 $reg
  set_property size           [expr {4*8}]   $reg

set reg      [::ipx::add_register -quiet "bar" $addr_block]
  set_property address_offset 0x4 $reg
  set_property size           [expr {4*8}]   $reg

set_property slave_memory_map_ref "s_axi_control" [::ipx::get_bus_interfaces -of $core "s_axi_control"]

